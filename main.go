// Copyright (C) 2026 Mitchell Dalvi Rosen
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General
// Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License along with this program. If not, see
// <https://www.gnu.org/licenses/>.

package main

import (
	"context"
	"database/sql"
	"embed"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	_ "modernc.org/sqlite"
	"net/http"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"
)

//go:embed migrations/*.sql
var migrationFS embed.FS

//go:embed queries
var queriesFS embed.FS

var queries map[string]string

var shoppingDataDir = "/var/lib/shopping"
var shoppingAddr = ":80"

func init() {
	if v := os.Getenv("SHOPPING_DATA_DIR"); v != "" {
		shoppingDataDir = v
	}
	if v := os.Getenv("SHOPPING_ADDR"); v != "" {
		shoppingAddr = v
	}

	entries, err := fs.ReadDir(queriesFS, "queries")
	if err != nil {
		fmt.Fprintf(os.Stderr, "reading queries directory: %v\n", err)
		os.Exit(1)
	}

	queries = make(map[string]string, len(entries))
	for _, e := range entries {
		var content []byte
		content, err = queriesFS.ReadFile("queries/" + e.Name())
		if err != nil {
			fmt.Fprintf(os.Stderr, "reading %s: %v\n", e.Name(), err)
			os.Exit(1)
		}
		queries[strings.TrimSuffix(e.Name(), ".sql")] = string(content)
	}
}

func main() {
	var err error

	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "usage: %s <queries|schema|serve>\n", os.Args[0])
		os.Exit(1)
	}

	var ctx context.Context = context.Background()

	switch os.Args[1] {
	case "queries":
		main_queries()
	case "schema":
		err = main_schema(ctx)
	case "serve":
		err = main_serve(ctx)
	default:
		fmt.Fprintf(os.Stderr, "usage: %s <queries|schema|serve>\n", os.Args[0])
		os.Exit(1)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}

// Print all queries
func main_queries() {

	var names []string = make([]string, 0, len(queries))
	for name := range queries {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		fmt.Printf("-- %s.sql\n%s\n\n", name, strings.TrimSpace(queries[name]))
	}
}

// Print the database schema
func main_schema(ctx context.Context) error {
	db, err := main_helper_open_database(ctx, queries)
	if err != nil {
		return err
	}
	defer db.Close()

	rows, err := db.Query(queries["get_schema"])
	if err != nil {
		return err
	}
	defer rows.Close()

	for rows.Next() {
		var stmt string
		err = rows.Scan(&stmt)
		if err != nil {
			return fmt.Errorf("scanning schema row: %w", err)
		}
		fmt.Printf("%s;\n", stmt)
	}
	return rows.Err()
}

func main_serve(ctx context.Context) error {
	db, err := main_helper_open_database(ctx, queries)
	if err != nil {
		return err
	}
	defer db.Close()

	mux := http.NewServeMux()

	// Single-page app routes

	serveIndexHtml := func(pattern string) {
		mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
			response.Header().Set("Cache-Control", "no-cache")
			http.ServeFile(response, request, "index.html")
		})
	}

	serveIndexHtml("GET /")
	serveIndexHtml("GET /item/{item}")
	serveIndexHtml("GET /item/{item}/store/{store}")
	serveIndexHtml("GET /items")
	serveIndexHtml("GET /location/{location}")
	serveIndexHtml("GET /shop")
	serveIndexHtml("GET /shop/{store}")
	serveIndexHtml("GET /store/{store}")
	serveIndexHtml("GET /store/{store}/item/{item}")
	serveIndexHtml("GET /stores")

	// Static files

	serveStaticFile := func(pattern string, file string) {
		mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
			http.ServeFile(response, request, file)
		})
	}

	serveStaticFile("GET /favicon.svg", "favicon.svg")
	serveStaticFile("GET /index.js", "index.js")
	serveStaticFile("GET /main.css", "main.css")
	serveStaticFile("GET /main.js", "main.js")
	mux.HandleFunc("GET /manifest.webmanifest", func(response http.ResponseWriter, request *http.Request) {
		response.Header().Set("Content-Type", "application/manifest+json")
		http.ServeFile(response, request, "manifest.webmanifest")
	})

	// API routes

	defineHandler := func(pattern string, handler func(*Handler)) {
		mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
			handler(NewHandler(db, response, request))
		})
	}

	defineHandler("GET /api/items", handleGetItems)
	defineHandler("POST /api/create-item", handleCreateItem)
	defineHandler("POST /api/create-location", handleCreateLocation)
	defineHandler("POST /api/create-store", handleCreateStore)
	defineHandler("POST /api/delete-item", handleDeleteItem)
	defineHandler("POST /api/delete-location", handleDeleteLocation)
	defineHandler("POST /api/delete-store", handleDeleteStore)
	defineHandler("POST /api/item-in-store", handleItemInStore)
	defineHandler("POST /api/item-not-in-store", handleItemNotInStore)
	defineHandler("POST /api/item-off", handleItemOff)
	defineHandler("POST /api/item-on", handleItemOn)
	defineHandler("POST /api/rename-item", handleRenameItem)
	defineHandler("POST /api/rename-location", handleRenameLocation)
	defineHandler("POST /api/rename-store", handleRenameStore)
	defineHandler("POST /api/reorder-locations", handleReorderLocations)

	slog.Info("server running", "addr", shoppingAddr)
	return http.ListenAndServe(shoppingAddr, requestLoggingMiddleware(mux))
}

// Serve an immutable file. This (annoyingly) unused, but becomes used after the build process, which does some
// hashing and renaming. See Dockerfile.
func serveHashedStaticFile(mux *http.ServeMux, pattern string, file string) {
	mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
		response.Header().Set("Cache-Control", "max-age=31536000, immutable")
		http.ServeFile(response, request, file)
	})
}

func main_helper_open_database(ctx context.Context, queries map[string]string) (*sql.DB, error) {
	path := filepath.Join(shoppingDataDir, "shopping.db")

	// Determine whether we are creating a new database file.
	isNew := false
	_, err := os.Stat(path)
	if os.IsNotExist(err) {
		isNew = true
	} else if err != nil {
		return nil, fmt.Errorf("checking database file: %w\n", err)
	}

	db, err := dbOpen(path)
	if err != nil {
		return nil, err
	}
	defer func() {
		if err != nil {
			_ = db.Close()
		}
	}()

	// Determine current schema version.
	currentSchemaVersion := -1
	if !isNew {
		currentSchemaVersion, err = dbQueryRowInt(ctx, queries, db, "get_schema_version")
		if err != nil {
			return db, err
		}
	}

	// Read migration files, keeping only those newer than the current version.
	entries, err := fs.ReadDir(migrationFS, "migrations")
	if err != nil {
		return db, fmt.Errorf("reading migrations directory: %w\n", err)
	}

	migrations := []string{}
	highest := currentSchemaVersion
	for _, entry := range entries {
		var n int
		name := entry.Name()
		n, err = strconv.Atoi(strings.TrimSuffix(name, ".sql"))
		if err != nil {
			return db, fmt.Errorf("parsing %v as int: %w\n", entry, err)
		}
		if n > currentSchemaVersion {
			migrations = append(migrations, name)
			highest = n
		}
	}

	// Run migrations.
	for _, name := range migrations {
		slog.Info("running migration", "name", name)

		bytes, err := migrationFS.ReadFile("migrations/" + name)
		if err != nil {
			return db, fmt.Errorf("reading migration %s: %w\n", name, err)
		}

		tx, err := dbBeginTx(ctx, db)
		if err != nil {
			return db, err
		}
		defer func() {
			if err != nil {
				_ = tx.Rollback()
			}
		}()

		_, err = tx.ExecContext(ctx, string(bytes))
		if err != nil {
			return db, fmt.Errorf("executing migration %s: %w\n", name, err)
		}

		err = tx.Commit()
		if err != nil {
			return db, err
		}
	}

	// Update schema_version if any migrations were applied.
	if highest > currentSchemaVersion {
		_, err = dbExec(ctx, queries, db, "update_schema_version", highest)
		if err != nil {
			return db, err
		}
	}

	return db, nil
}

// GET /api/items
func handleGetItems(handler *Handler) {
	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Get data version first (to support If-None-Match check)
	dataVersion, err := getDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Check If-None-Match header; if the client's version matches, return 304 Not Modified
	if handler.request.Header.Get("If-None-Match") == fmt.Sprintf(`"%d"`, dataVersion) {
		handler.response.WriteHeader(http.StatusNotModified)
		return
	}

	// Read entire items table
	rows, err := handler.Query("get_items")
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type item struct {
		Id     int64  `json:"id"`
		Name   string `json:"name"`
		OnList bool   `json:"on_list"`
	}
	items := []item{}
	for rows.Next() {
		var item item
		err = rows.Scan(&item.Id, &item.Name, &item.OnList)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		items = append(items, item)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Read entire stores table
	rows, err = handler.Query("get_stores")
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type store struct {
		Id   int64  `json:"id"`
		Name string `json:"name"`
	}
	stores := []store{}
	for rows.Next() {
		var store store
		err = rows.Scan(&store.Id, &store.Name)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		stores = append(stores, store)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Read entire locations table
	rows, err = handler.Query("get_locations")
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type location struct {
		Id       int64  `json:"id"`
		Store    int64  `json:"store"`
		Position int64  `json:"position"`
		Name     string `json:"name"`
	}
	locations := []location{}
	for rows.Next() {
		var location location
		err = rows.Scan(&location.Id, &location.Store, &location.Position, &location.Name)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		locations = append(locations, location)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Read entire item_stores table
	rows, err = handler.Query("get_item_stores")
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type itemStore struct {
		Item     int64  `json:"item"`
		Store    int64  `json:"store"`
		Sold     bool   `json:"sold"`
		Location *int64 `json:"location"`
	}
	itemStores := []itemStore{}
	for rows.Next() {
		var itemStore itemStore
		err = rows.Scan(&itemStore.Item, &itemStore.Store, &itemStore.Sold, &itemStore.Location)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		itemStores = append(itemStores, itemStore)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64       `json:"data_version"`
		Items       []item      `json:"items"`
		Stores      []store     `json:"stores"`
		Locations   []location  `json:"locations"`
		ItemStores  []itemStore `json:"item_stores"`
	}{
		DataVersion: dataVersion,
		Items:       items,
		Stores:      stores,
		Locations:   locations,
		ItemStores:  itemStores})
}

// POST /api/create-item
//
// Create a new item, and optionally, record it as being sold in a specific store.
func handleCreateItem(handler *Handler) {
	var requestBody struct {
		Name   string `json:"name"`
		OnList bool   `json:"on_list"`
		Store  *int64 `json:"store"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	name := strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Confirm an item with that name doesn't already exist
	exists, err := existsItemByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Create item
	itemId, err := insertItem(handler, name, requestBody.OnList)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Possibly record new item as sold in a store
	if requestBody.Store != nil {
		_, err := upsertItemStore(handler, itemId, *requestBody.Store, true, nil)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	handler.response.WriteHeader(http.StatusCreated)
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
	}{
		DataVersion: dataVersion,
		Id:          itemId})
}

// POST /api/create-location
func handleCreateLocation(handler *Handler) {
	var requestBody struct {
		Store int64  `json:"store"`
		Name  string `json:"name"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}
	var name string = strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Create location
	id, position, err := insertLocation(handler, requestBody.Store, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	handler.response.WriteHeader(http.StatusCreated)
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
		Position    int64 `json:"position"`
	}{
		DataVersion: dataVersion,
		Id:          id,
		Position:    position})
}

// POST /api/create-store
func handleCreateStore(handler *Handler) {
	var requestBody struct {
		Name string `json:"name"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}
	var name string = strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Confirm a store with that name doesn't already exist
	exists, err := handler.QueryRowBool("exists_store_by_name", name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Create store
	id, err := handler.QueryRowInt64("insert_store", name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	handler.response.WriteHeader(http.StatusCreated)
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
	}{
		DataVersion: dataVersion,
		Id:          id})
}

// POST /api/delete-item
func handleDeleteItem(handler *Handler) {
	var requestBody struct {
		Id int64 `json:"id"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Delete item
	result, err := deleteItem(handler, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If nothing was deleted, 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/delete-location
func handleDeleteLocation(handler *Handler) {
	var requestBody struct {
		Id int64 `json:"id"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Delete location
	result, err := deleteLocation(handler, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If nothing was deleted, 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/delete-store
func handleDeleteStore(handler *Handler) {
	var requestBody struct {
		Id int64 `json:"id"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Delete store
	result, err := deleteStore(handler, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If nothing was deleted, 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/item-in-store
//
// Record that an item is sold at a store, and optionally, which location within the store.
func handleItemInStore(handler *Handler) {
	var requestBody struct {
		Item     int64  `json:"item"`
		Store    int64  `json:"store"`
		Location *int64 `json:"location"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Confirm the item/store/location all exist, and that the store/location correspond.
	itemExists, err := existsItemById(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !itemExists {
		handler.SendConflict()
		return
	}
	if requestBody.Location == nil {
		storeExists, err := existsStoreById(handler, requestBody.Store)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		if !storeExists {
			handler.SendConflict()
			return
		}
	} else {
		storeLocationExists, err := existsLocationByStoreIdLocationId(handler, requestBody.Store, *requestBody.Location)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		if !storeLocationExists {
			handler.SendConflict()
			return
		}
	}

	// Upsert the item_store row
	_, err = upsertItemStore(handler, requestBody.Item, requestBody.Store, true, requestBody.Location)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/item-not-in-store
//
// Record that an item is not sold at a store.
func handleItemNotInStore(handler *Handler) {
	var requestBody struct {
		Item  int64 `json:"item"`
		Store int64 `json:"store"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Confirm the item and store exist.
	itemExists, err := existsItemById(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !itemExists {
		handler.SendConflict()
		return
	}
	storeExists, err := existsStoreById(handler, requestBody.Store)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !storeExists {
		handler.SendConflict()
		return
	}

	// Upsert the item_store row
	_, err = upsertItemStore(handler, requestBody.Item, requestBody.Store, false, nil)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/item-off
//
// Move an existing item off the shopping list, and optionally, also record it as being sold at a store, possibly at a
// specific location. The store/location are purely additive information, so if (for whatever reason) a store is
// provided but a location isn't, if there's already a known location, it won't be deleted.
func handleItemOff(handler *Handler) {
	// Decode request body
	var requestBody struct {
		Item     int64  `json:"item"`
		Store    *int64 `json:"store"`
		Location *int64 `json:"location"`
	}
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Move item off shopping list
	result, err := itemOffList(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If no rows affected (item doesn't exist), 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// If store/location provided, record that the item is sold at that store/location.
	// Nil store with non-nil location is nonsense, but we don't even check that, just ignore it.
	if requestBody.Store != nil {
		shouldUpsertItemStore := true
		if requestBody.Location != nil {
			storeLocationExists, err := existsLocationByStoreIdLocationId(handler, *requestBody.Store, *requestBody.Location)
			if err != nil {
				handler.InternalServerError(err)
				return
			}
			if !storeLocationExists {
				handler.SendConflict()
				return
			}
		} else {
			storeExists, err := existsStoreById(handler, *requestBody.Store)
			if err != nil {
				handler.InternalServerError(err)
				return
			}
			if !storeExists {
				handler.SendConflict()
				return
			}
			// Corner case - if store is provided but location isn't, don't want to overwrite existing
			// store+location with store+nil
			hasLocation, err := itemStoreHasLocation(handler, requestBody.Item, *requestBody.Store)
			if err != nil {
				handler.InternalServerError(err)
				return
			}
			if hasLocation {
				shouldUpsertItemStore = false
			}
		}
		if shouldUpsertItemStore {
			_, err = upsertItemStore(handler, requestBody.Item, *requestBody.Store, true, requestBody.Location)
			if err != nil {
				handler.InternalServerError(err)
				return
			}
		}
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/item-on
//
// Move an existing item on the shopping list.
func handleItemOn(handler *Handler) {
	// Decode request body
	var requestBody struct {
		Item int64 `json:"item"`
	}
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Move item on shopping list
	result, err := itemOnList(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If no rows affected (item doesn't exist), 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/rename-item
func handleRenameItem(handler *Handler) {
	var requestBody struct {
		Id   int64  `json:"id"`
		Name string `json:"name"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}
	var name string = strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Get whether an item already exists with the requested name. If it does, 409. (Note that this also 409s in the
	// case that the item itself has this name - that's okay).
	exists, err := existsItemByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Update this item's name to the requested name
	_, err = updateItemName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/rename-location
func handleRenameLocation(handler *Handler) {
	var requestBody struct {
		Id    int64  `json:"id"`
		Store int64  `json:"store"`
		Name  string `json:"name"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}
	var name string = strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Update this location's name to the requested name
	result, err := updateLocationName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If location doesn't exist (so no row updated), 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/rename-store
func handleRenameStore(handler *Handler) {
	var requestBody struct {
		Id   int64  `json:"id"`
		Name string `json:"name"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}
	var name string = strings.TrimSpace(requestBody.Name)
	if name == "" {
		handler.SendBadRequest("empty name")
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Get whether a store already exists with the requested name. If it does, 409. (Note that this also 409s in the
	// case that the store itself has this name - that's okay).
	exists, err := existsStoreByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Update this store's name to the requested name
	_, err = updateStoreName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// POST /api/reorder-locations
func handleReorderLocations(handler *Handler) {
	var requestBody struct {
		Store     int64   `json:"store"`
		Locations []int64 `json:"locations"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.BeginTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.RollbackTx()

	// Confirm the provided location ids are a permutation of the store's locations
	rows, err := getLocationIdsByStore(handler, requestBody.Store)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	var theLocations []int64
	for rows.Next() {
		var location int64
		err = rows.Scan(&location)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		theLocations = append(theLocations, location)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !slices.Equal(theLocations, slices.Sorted(slices.Values(requestBody.Locations))) {
		handler.SendConflict()
		return
	}

	// Update all location positions to their position in the list
	for position, location := range requestBody.Locations {
		_, err = updateLocationPosition(handler, int64(position), location, requestBody.Store)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
	}

	// Bump data version
	dataVersion, err := bumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.CommitTx()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	handler.response.Header().Set("Content-Type", "application/json")
	json.NewEncoder(handler.response).Encode(struct {
		DataVersion int64 `json:"data_version"`
	}{
		DataVersion: dataVersion})
}

// Query wrappers

func bumpDataVersion(handler *Handler) (int64, error) {
	return handler.QueryRowInt64("bump_data_version")
}

func deleteItem(handler *Handler, id int64) (sql.Result, error) {
	return handler.Exec("delete_item", id)
}

func deleteLocation(handler *Handler, id int64) (sql.Result, error) {
	return handler.Exec("delete_location", id)
}

func deleteStore(handler *Handler, id int64) (sql.Result, error) {
	return handler.Exec("delete_store", id)
}

func existsItemById(handler *Handler, id int64) (bool, error) {
	return handler.QueryRowBool("exists_item_by_id", id)
}

func existsItemByName(handler *Handler, name string) (bool, error) {
	return handler.QueryRowBool("exists_item_by_name", name)
}

func existsLocationByStoreIdLocationId(handler *Handler, storeId int64, locationId int64) (bool, error) {
	return handler.QueryRowBool("exists_location_by_store_id_location_id", storeId, locationId)
}

func existsStoreById(handler *Handler, id int64) (bool, error) {
	return handler.QueryRowBool("exists_store_by_id", id)
}

func existsStoreByName(handler *Handler, name string) (bool, error) {
	return handler.QueryRowBool("exists_store_by_name", name)
}

func getDataVersion(handler *Handler) (int64, error) {
	return handler.QueryRowInt64("get_data_version")
}

func getLocationIdsByStore(handler *Handler, storeId int64) (*sql.Rows, error) {
	return handler.Query("get_location_ids_by_store", storeId)
}

func insertItem(handler *Handler, name string, onList bool) (int64, error) {
	return handler.QueryRowInt64("insert_item", name, onList)
}

func insertLocation(handler *Handler, storeId int64, name string) (int64, int64, error) {
	return handler.QueryRowInt64Int64("insert_location", storeId, storeId, name)
}

func itemOffList(handler *Handler, id int64) (sql.Result, error) {
	return handler.Exec("item_off_list", id)
}

func itemOnList(handler *Handler, id int64) (sql.Result, error) {
	return handler.Exec("item_on_list", id)
}

func itemStoreHasLocation(handler *Handler, itemId int64, storeId int64) (bool, error) {
	return handler.QueryRowBool("item_store_has_location", itemId, storeId)
}

func updateItemName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.Exec("update_item_name", name, id)
}

func updateLocationName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.Exec("update_location_name", name, id)
}

func updateLocationPosition(handler *Handler, position int64, id int64, store int64) (sql.Result, error) {
	return handler.Exec("update_location_position", position, id, store)
}

func updateStoreName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.Exec("update_store_name", name, id)
}

func upsertItemStore(handler *Handler, itemId int64, storeId int64, sold bool, locationId *int64) (sql.Result, error) {
	return handler.Exec("upsert_item_store", itemId, storeId, sold, locationId)
}

// Request-logging middleware

type responseWriterThatRemembersStatus struct {
	http.ResponseWriter // embedded; methods promoted to wrapper type
	status              int
}

func (writer *responseWriterThatRemembersStatus) WriteHeader(status int) {
	writer.status = status
	writer.ResponseWriter.WriteHeader(status)
}

func requestLoggingMiddleware(innerHandler http.Handler) http.Handler {
	handler := func(response http.ResponseWriter, request *http.Request) {
		response2 :=
			&responseWriterThatRemembersStatus{
				ResponseWriter: response,
				status:         http.StatusOK}
		t0 := time.Now()
		innerHandler.ServeHTTP(response2, request)
		t1 := time.Now()
		slog.Info(
			"request",
			"method", request.Method,
			"path", request.URL.Path,
			"status", response2.status,
			"duration", t1.Sub(t0))
	}
	return http.HandlerFunc(handler)
}

// Handler abstraction

type Handler struct {
	committed bool // Whether the current transaction has committed
	db        *sql.DB
	logger    *slog.Logger
	request   *http.Request
	response  http.ResponseWriter
	tx        *sql.Tx // The current transaction
}

func NewHandler(db *sql.DB, response http.ResponseWriter, request *http.Request) *Handler {
	return &Handler{
		committed: false,
		db:        db,
		logger:    slog.Default(), // could use
		request:   request,
		response:  response,
		tx:        nil}
}

func (handler *Handler) BeginTx() error {
	tx, err := dbBeginTx(handler.request.Context(), handler.db)
	if err != nil {
		return err
	}
	handler.committed = false
	handler.tx = tx
	return nil
}

func (handler *Handler) CommitTx() error {
	err := dbCommitTx(handler.tx)
	if err == nil {
		handler.committed = true
	}
	return err
}

func (handler *Handler) DecodeJsonRequestBody(v any) bool {
	err := json.NewDecoder(handler.request.Body).Decode(v)

	if err != nil {
		http.Error(handler.response, err.Error(), http.StatusBadRequest)
		return true
	}

	return false
}

func (handler *Handler) InternalServerError(err error) {
	handler.logger.Error("Unexpected error", "error", err)
	http.Error(handler.response, "", http.StatusInternalServerError)
}

func (handler *Handler) SendBadRequest(message string) {
	http.Error(handler.response, message, http.StatusBadRequest)
}

func (handler *Handler) SendConflict() {
	http.Error(handler.response, "", http.StatusConflict)
}

func (handler *Handler) SendOk() {
	handler.response.WriteHeader(http.StatusOK)
}

func (handler *Handler) Exec(name string, args ...any) (sql.Result, error) {
	return handler.tx.ExecContext(handler.request.Context(), queries[name], args...)
}

func (handler *Handler) QueryRow(name string, args ...any) *sql.Row {
	return handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
}

func (handler *Handler) QueryRowBool(name string, args ...any) (bool, error) {
	row := handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
	var x bool
	err := row.Scan(&x)
	return x, err
}

func (handler *Handler) QueryRowInt64(name string, args ...any) (int64, error) {
	row := handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
	var x int64
	err := row.Scan(&x)
	return x, err
}

func (handler *Handler) QueryRowInt64Int64(name string, args ...any) (int64, int64, error) {
	row := handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
	var x int64
	var y int64
	err := row.Scan(&x, &y)
	return x, y, err
}

func (handler *Handler) QueryMaybeRowInt64(name string, args ...any) (*int64, error) {
	row := handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
	var x int64
	err := row.Scan(&x)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &x, nil
}

func (handler *Handler) QueryMaybeRowString(name string, args ...any) (*string, error) {
	row := handler.tx.QueryRowContext(handler.request.Context(), queries[name], args...)
	var x string
	err := row.Scan(&x)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &x, nil
}

func (handler *Handler) Query(name string, args ...any) (*sql.Rows, error) {
	return handler.tx.QueryContext(handler.request.Context(), queries[name], args...)
}

// Rollback a transaction if it hasn't been committed. To always be deferred immediately after a successful BeginTx().
func (handler *Handler) RollbackTx() {
	if !handler.committed {
		_ = handler.tx.Rollback()
	}
}

// Database abstraction

func dbOpen(path string) (db *sql.DB, err error) {
	// Open the database file.
	db, err = sql.Open("sqlite", path)
	if err != nil {
		return db, fmt.Errorf("opening database file %s: %w\n", path, err)
	}
	defer func() {
		if err != nil {
			_ = db.Close()
		}
	}()

	// Use up to 1 connection, don't close it when idle. By literally serializing all writes, we get to avoid
	// writing retry-on-busy loops that we'd otherwise get in the presence of concurrent writes (which should be
	// very rare anyway). Our queries are extremely small and fast, so serializing writes is totally fine.
	db.SetConnMaxLifetime(0)
	db.SetMaxIdleConns(1)
	db.SetMaxOpenConns(1)

	// Enable WAL mode (persists on database, but fine to set again and again).
	_, err = db.Exec("PRAGMA journal_mode = WAL")
	if err != nil {
		return db, fmt.Errorf("setting journal mode to WAL: %w\n", err)
	}

	// Enable foreign key integrity checking on the connection.
	_, err = db.Exec("PRAGMA foreign_keys = ON")
	if err != nil {
		return db, fmt.Errorf("enabling foreign keys: %w\n", err)
	}

	return db, nil
}

func dbBeginTx(ctx context.Context, db *sql.DB) (*sql.Tx, error) {
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		return nil, fmt.Errorf("beginning transaction: %w\n", err)
	}
	return tx, nil
}

func dbCommitTx(tx *sql.Tx) error {
	err := tx.Commit()
	if err != nil {
		return fmt.Errorf("committing transaction: %w\n", err)
	}
	return nil
}

func dbExec(ctx context.Context, queries map[string]string, db *sql.DB, name string, args ...any) (sql.Result, error) {
	result, err := db.ExecContext(ctx, queries[name], args...)
	if err != nil {
		return result, fmt.Errorf("%s: %w\n", name, err)
	}
	return result, nil
}

func dbQueryRowInt(ctx context.Context, queries map[string]string, db *sql.DB, name string, args ...any) (int, error) {
	row := db.QueryRowContext(ctx, queries[name], args...)
	var n int
	err := row.Scan(&n)
	if err != nil {
		return n, fmt.Errorf("%s: %w\n", name, err)
	}
	return n, nil
}

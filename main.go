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
	"strconv"
	"strings"
	"time"
)

//go:embed migrations/*.sql
var migrationFS embed.FS

type queryKey int

const (
	queryKeyBumpDataVersion queryKey = iota
	queryKeyDeleteItem
	queryKeyDeleteSection
	queryKeyDeleteStore
	queryKeyExistsItemById
	queryKeyExistsItemByName
	queryKeyExistsSectionByStoreIdSectionId
	queryKeyExistsStoreById
	queryKeyExistsStoreByName
	queryKeyGetDataVersion
	queryKeyGetItemStores
	queryKeyGetItems
	queryKeyGetSectionIdsByStore
	queryKeyGetSections
	queryKeyGetStores
	queryKeyInsertItem
	queryKeyInsertSection
	queryKeyInsertStore
	queryKeyItemOffList
	queryKeyItemOnList
	queryKeyItemStoreHasSection
	queryKeyUpdateItemName
	queryKeyUpdateSectionName
	queryKeyUpdateSectionPosition
	queryKeyUpdateStoreName
	queryKeyUpsertItemStore
)

var queries = map[queryKey]string{
	queryKeyBumpDataVersion:                 "UPDATE data_version SET version = version + 1 RETURNING version",
	queryKeyDeleteItem:                      "DELETE FROM items WHERE id = ?",
	queryKeyDeleteSection:                   "DELETE FROM sections WHERE id = ?",
	queryKeyDeleteStore:                     "DELETE FROM stores WHERE id = ?",
	queryKeyExistsItemById:                  "SELECT EXISTS (SELECT 1 FROM items WHERE id = ?)",
	queryKeyExistsItemByName:                "SELECT EXISTS (SELECT 1 FROM items WHERE name = ?)",
	queryKeyExistsSectionByStoreIdSectionId: "SELECT EXISTS (SELECT 1 FROM sections WHERE store = ? AND id = ?)",
	queryKeyExistsStoreById:                 "SELECT EXISTS (SELECT 1 FROM stores WHERE id = ?)",
	queryKeyExistsStoreByName:               "SELECT EXISTS (SELECT 1 FROM stores WHERE name = ?)",
	queryKeyGetDataVersion:                  "SELECT version FROM data_version",
	queryKeyGetItemStores:                   "SELECT item, store, sold, section FROM item_stores",
	queryKeyGetItems:                        "SELECT id, name, on_list FROM items",
	queryKeyGetSectionIdsByStore:            "SELECT id FROM sections WHERE store = ? ORDER BY id",
	queryKeyGetSections:                     "SELECT id, store, position, name FROM sections",
	queryKeyGetStores:                       "SELECT id, name FROM stores",
	queryKeyInsertItem:                      "INSERT INTO items (name, on_list) VALUES (?, ?) RETURNING id",
	queryKeyInsertSection:                   "INSERT INTO sections (store, position, name) VALUES (?, COALESCE((SELECT MAX(position) + 1 FROM sections WHERE store = ?), 0), ?) RETURNING id, position",
	queryKeyInsertStore:                     "INSERT INTO stores (name) VALUES (?) ON CONFLICT (name) DO NOTHING RETURNING id",
	queryKeyItemOffList:                     "UPDATE items SET on_list = 0 WHERE id = ?",
	queryKeyItemOnList:                      "UPDATE items SET on_list = 1 WHERE id = ?",
	queryKeyItemStoreHasSection:             "SELECT EXISTS (SELECT 1 FROM item_stores WHERE item = ? AND store = ? AND section IS NOT NULL)",
	queryKeyUpdateItemName:                  "UPDATE items SET name = ? WHERE id = ?",
	queryKeyUpdateSectionName:               "UPDATE sections SET name = ? WHERE id = ?",
	queryKeyUpdateSectionPosition:           "UPDATE sections SET position = ? WHERE id = ? AND store = ?",
	queryKeyUpdateStoreName:                 "UPDATE stores SET name = ? WHERE id = ?",
	queryKeyUpsertItemStore:                 "INSERT INTO item_stores (item, store, sold, section) SELECT ?, ?, ?, ? ON CONFLICT (item, store) DO UPDATE SET sold = excluded.sold, section = excluded.section",
}

var preparedQueries = map[queryKey]*sql.Stmt{}

var shoppingDataDir = "/var/lib/shopping"
var shoppingAddr = ":80"

func init() {
	if v := os.Getenv("SHOPPING_DATA_DIR"); v != "" {
		shoppingDataDir = v
	}
	if v := os.Getenv("SHOPPING_ADDR"); v != "" {
		shoppingAddr = v
	}
}

func main() {
	err := main_serve()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}

func main_serve() error {
	path := filepath.Join(shoppingDataDir, "shopping.db")

	// Determine whether we are creating a new database file.
	isNew := false
	_, err := os.Stat(path)
	if os.IsNotExist(err) {
		isNew = true
	} else if err != nil {
		return fmt.Errorf("checking database file: %w\n", err)
	}

	// Open the database file.
	db, err := sql.Open("sqlite", path)
	if err != nil {
		return fmt.Errorf("opening database file %s: %w\n", path, err)
	}
	defer db.Close()

	// Use up to 1 connection, don't close it when idle. By literally serializing all writes, we get to avoid
	// writing retry-on-busy loops that we'd otherwise get in the presence of concurrent writes (which should be
	// very rare anyway). Our queries are extremely small and fast, so serializing writes is totally fine. Using
	// only 1 connection also allows us to just enable foreign keys once. If we want multiple connections, we'd
	// have to write some annoying wrapper logic that acts as a "open connection hook".
	db.SetConnMaxLifetime(0)
	db.SetMaxIdleConns(1)
	db.SetMaxOpenConns(1)

	// Enable WAL mode (persists on database, but fine to set again and again).
	_, err = db.Exec("PRAGMA journal_mode = WAL")
	if err != nil {
		return fmt.Errorf("setting journal mode to WAL: %w\n", err)
	}

	// Enable foreign key integrity checking on the connection.
	_, err = db.Exec("PRAGMA foreign_keys = ON")
	if err != nil {
		return fmt.Errorf("enabling foreign keys: %w\n", err)
	}

	// Determine current schema version.
	currentSchemaVersion := -1
	if !isNew {
		row := db.QueryRow("SELECT version FROM schema_version")
		err := row.Scan(&currentSchemaVersion)
		if err != nil {
			return err
		}
	}

	// Read migration files, keeping only those newer than the current version.
	entries, err := fs.ReadDir(migrationFS, "migrations")
	if err != nil {
		return fmt.Errorf("reading migrations directory: %w\n", err)
	}

	migrations := []string{}
	highest := currentSchemaVersion
	for _, entry := range entries {
		var n int
		name := entry.Name()
		n, err = strconv.Atoi(strings.TrimSuffix(name, ".sql"))
		if err != nil {
			return fmt.Errorf("parsing %v as int: %w\n", entry, err)
		}
		if n > currentSchemaVersion {
			migrations = append(migrations, name)
			highest = n
		}
	}

	// Run migrations.
	for _, name := range migrations {
		if !isNew {
			slog.Info("running migration", "name", name)
		}

		bytes, err := migrationFS.ReadFile("migrations/" + name)
		if err != nil {
			return fmt.Errorf("reading migration %s: %w\n", name, err)
		}

		tx, err := db.Begin()
		if err != nil {
			return err
		}
		defer tx.Rollback()

		_, err = tx.Exec(string(bytes))
		if err != nil {
			return fmt.Errorf("executing migration %s: %w\n", name, err)
		}

		err = tx.Commit()
		if err != nil {
			return err
		}
	}

	// Update schema_version if any migrations were applied.
	if highest > currentSchemaVersion {
		_, err = db.Exec("UPDATE schema_version SET version = ?", highest)
		if err != nil {
			return err
		}
	}

	// Prepare queries
	for key, query := range queries {
		stmt, err := db.Prepare(query)
		if err != nil {
			return err
		}
		defer stmt.Close()
		preparedQueries[key] = stmt
	}

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
	serveIndexHtml("GET /shop")
	serveIndexHtml("GET /shop/{store}")
	serveIndexHtml("GET /shop/{store}/item/{item}")
	serveIndexHtml("GET /store/{store}")
	serveIndexHtml("GET /store/{store}/item/{item}")
	serveIndexHtml("GET /store/{store}/section/{section}")
	serveIndexHtml("GET /store/{store}/section/{section}/item/{item}")
	serveIndexHtml("GET /stores")

	// Static files

	serveStaticFile(mux, "GET /favicon.svg", "image/svg+xml", "favicon.svg")
	serveStaticFile(mux, "GET /index.js", "text/javascript", "index.js")
	serveStaticFile(mux, "GET /main.css", "text/css", "main.css")
	serveStaticFile(mux, "GET /main.js", "text/javascript", "main.js")
	serveStaticFile(mux, "GET /manifest.webmanifest", "application/manifest+json", "manifest.webmanifest")

	// API routes

	defineHandler := func(pattern string, handler func(*Handler)) {
		mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
			handler(NewHandler(db, response, request))
		})
	}

	defineHandler("GET /api/items", handleGetItems)
	defineHandler("POST /api/create-item", handleCreateItem)
	defineHandler("POST /api/create-section", handleCreateSection)
	defineHandler("POST /api/create-store", handleCreateStore)
	defineHandler("POST /api/delete-item", handleDeleteItem)
	defineHandler("POST /api/delete-section", handleDeleteSection)
	defineHandler("POST /api/delete-store", handleDeleteStore)
	defineHandler("POST /api/item-in-store", handleItemInStore)
	defineHandler("POST /api/item-not-in-store", handleItemNotInStore)
	defineHandler("POST /api/item-off", handleItemOff)
	defineHandler("POST /api/item-on", handleItemOn)
	defineHandler("POST /api/rename-item", handleRenameItem)
	defineHandler("POST /api/rename-section", handleRenameSection)
	defineHandler("POST /api/rename-store", handleRenameStore)
	defineHandler("POST /api/reorder-sections", handleReorderSections)

	slog.Info("server running", "addr", shoppingAddr)
	return http.ListenAndServe(shoppingAddr, crashOnPanicMiddleware(requestLoggingMiddleware(mux)))
}

func serveStaticFile(mux *http.ServeMux, pattern string, contentType string, file string) {
	mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
		response.Header().Set("Content-Type", contentType)
		http.ServeFile(response, request, file)
	})
}

// Serve an immutable file. This is unused, but becomes used after the build process, which does some hashing and
// renaming. See Dockerfile.
func serveHashedStaticFile(mux *http.ServeMux, pattern string, contentType string, file string) {
	mux.HandleFunc(pattern, func(response http.ResponseWriter, request *http.Request) {
		response.Header().Set("Cache-Control", "max-age=31536000, immutable")
		response.Header().Set("Content-Type", contentType)
		http.ServeFile(response, request, file)
	})
}

// GET /api/items
func handleGetItems(handler *Handler) {
	// Begin transaction
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Get data version first (to support If-None-Match check)
	dataVersion, err := sqliteGetDataVersion(handler)
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
	rows, err := handler.SqliteQuery_ManyRows(queryKeyGetItems)
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
	rows, err = handler.SqliteQuery_ManyRows(queryKeyGetStores)
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

	// Read entire sections table
	rows, err = handler.SqliteQuery_ManyRows(queryKeyGetSections)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type section struct {
		Id       int64  `json:"id"`
		Store    int64  `json:"store"`
		Position int64  `json:"position"`
		Name     string `json:"name"`
	}
	sections := []section{}
	for rows.Next() {
		var section section
		err = rows.Scan(&section.Id, &section.Store, &section.Position, &section.Name)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		sections = append(sections, section)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Read entire item_stores table
	rows, err = handler.SqliteQuery_ManyRows(queryKeyGetItemStores)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	type itemStore struct {
		Item    int64  `json:"item"`
		Store   int64  `json:"store"`
		Sold    bool   `json:"sold"`
		Section *int64 `json:"section"`
	}
	itemStores := []itemStore{}
	for rows.Next() {
		var itemStore itemStore
		err = rows.Scan(&itemStore.Item, &itemStore.Store, &itemStore.Sold, &itemStore.Section)
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
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64       `json:"data_version"`
		Items       []item      `json:"items"`
		Stores      []store     `json:"stores"`
		Sections    []section   `json:"sections"`
		ItemStores  []itemStore `json:"item_stores"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion,
			Items:       items,
			Stores:      stores,
			Sections:    sections,
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Confirm an item with that name doesn't already exist
	exists, err := sqliteExistsItemByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Create item
	itemId, err := sqliteInsertItem(handler, name, requestBody.OnList)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Possibly record new item as sold in a store
	if requestBody.Store != nil {
		_, err := sqliteUpsertItemStore(handler, itemId, *requestBody.Store, true, nil)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
	}
	handler.SendJsonResponse(
		http.StatusCreated,
		response{
			DataVersion: dataVersion,
			Id:          itemId})
}

// POST /api/create-section
func handleCreateSection(handler *Handler) {
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Create section
	id, position, err := sqliteInsertSection(handler, requestBody.Store, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
		Position    int64 `json:"position"`
	}
	handler.SendJsonResponse(
		http.StatusCreated,
		response{
			DataVersion: dataVersion,
			Id:          id,
			Position:    position})
}

// POST /api/create-store
//
// Create a new store, and optionally, record it as selling a specific item.
func handleCreateStore(handler *Handler) {
	var requestBody struct {
		Name string `json:"name"`
		Item *int64 `json:"item"`
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Confirm a store with that name doesn't already exist
	exists, err := sqliteExistsStoreByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Create store
	storeId, err := sqliteInsertStore(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Possibly record new store as selling an item
	if requestBody.Item != nil {
		_, err := sqliteUpsertItemStore(handler, *requestBody.Item, storeId, true, nil)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
		Id          int64 `json:"id"`
	}
	handler.SendJsonResponse(
		http.StatusCreated,
		response{
			DataVersion: dataVersion,
			Id:          storeId})
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Delete item
	result, err := sqliteDeleteItem(handler, requestBody.Id)
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
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// POST /api/delete-section
func handleDeleteSection(handler *Handler) {
	var requestBody struct {
		Id int64 `json:"id"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Delete section
	result, err := sqliteDeleteSection(handler, requestBody.Id)
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
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Delete store
	result, err := sqliteDeleteStore(handler, requestBody.Id)
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
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// POST /api/item-in-store
//
// Record that an item is sold at a store, and optionally, which section within the store.
func handleItemInStore(handler *Handler) {
	var requestBody struct {
		Item    int64  `json:"item"`
		Store   int64  `json:"store"`
		Section *int64 `json:"section"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Confirm the item/store/section all exist, and that the store/section correspond.
	itemExists, err := sqliteExistsItemById(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !itemExists {
		handler.SendConflict()
		return
	}
	if requestBody.Section == nil {
		storeExists, err := sqliteExistsStoreById(handler, requestBody.Store)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		if !storeExists {
			handler.SendConflict()
			return
		}
	} else {
		storeSectionExists, err := sqliteExistsSectionByStoreIdSectionId(
			handler,
			requestBody.Store,
			*requestBody.Section)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		if !storeSectionExists {
			handler.SendConflict()
			return
		}
	}

	// Upsert the item_store row
	_, err = sqliteUpsertItemStore(handler, requestBody.Item, requestBody.Store, true, requestBody.Section)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Confirm the item and store exist.
	itemExists, err := sqliteExistsItemById(handler, requestBody.Item)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !itemExists {
		handler.SendConflict()
		return
	}
	storeExists, err := sqliteExistsStoreById(handler, requestBody.Store)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !storeExists {
		handler.SendConflict()
		return
	}

	// Upsert the item_store row
	_, err = sqliteUpsertItemStore(handler, requestBody.Item, requestBody.Store, false, nil)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// POST /api/item-off
//
// Move an existing item off the shopping list.
func handleItemOff(handler *Handler) {
	// Decode request body
	var requestBody struct {
		Item int64 `json:"item"`
	}
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Move item off shopping list
	result, err := sqliteItemOffList(handler, requestBody.Item)
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
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Move item on shopping list
	result, err := sqliteItemOnList(handler, requestBody.Item)
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
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Get whether an item already exists with the requested name. If it does, 409. (Note that this also 409s in the
	// case that the item itself has this name - that's okay).
	exists, err := sqliteExistsItemByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Update this item's name to the requested name
	_, err = sqliteUpdateItemName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// POST /api/rename-section
func handleRenameSection(handler *Handler) {
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Update this section's name to the requested name
	result, err := sqliteUpdateSectionName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// If section doesn't exist (so no row updated), 409
	affected, _ := result.RowsAffected()
	if affected == 0 {
		handler.SendConflict()
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
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
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Get whether a store already exists with the requested name. If it does, 409. (Note that this also 409s in the
	// case that the store itself has this name - that's okay).
	exists, err := sqliteExistsStoreByName(handler, name)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if exists {
		handler.SendConflict()
		return
	}

	// Update this store's name to the requested name
	_, err = sqliteUpdateStoreName(handler, name, requestBody.Id)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// POST /api/reorder-sections
func handleReorderSections(handler *Handler) {
	var requestBody struct {
		Store    int64   `json:"store"`
		Sections []int64 `json:"sections"`
	}

	// Decode request body
	if handler.DecodeJsonRequestBody(&requestBody) {
		return
	}

	// Begin transaction
	err := handler.SqliteBeginTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer handler.SqliteRollbackTransaction()

	// Confirm the provided section ids are a permutation of the store's sections
	rows, err := sqliteGetSectionIdsByStore(handler, requestBody.Store)
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	defer rows.Close()
	var theSections []int64
	for rows.Next() {
		var section int64
		err = rows.Scan(&section)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
		theSections = append(theSections, section)
	}
	err = rows.Err()
	if err != nil {
		handler.InternalServerError(err)
		return
	}
	if !slices.Equal(theSections, slices.Sorted(slices.Values(requestBody.Sections))) {
		handler.SendConflict()
		return
	}

	// Update all section positions to their position in the list
	for position, section := range requestBody.Sections {
		_, err = sqliteUpdateSectionPosition(handler, int64(position), section, requestBody.Store)
		if err != nil {
			handler.InternalServerError(err)
			return
		}
	}

	// Bump data version
	dataVersion, err := sqliteBumpDataVersion(handler)
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Commit transaction
	err = handler.SqliteCommitTransaction()
	if err != nil {
		handler.InternalServerError(err)
		return
	}

	// Send response
	type response struct {
		DataVersion int64 `json:"data_version"`
	}
	handler.SendJsonResponse(
		http.StatusOK,
		response{
			DataVersion: dataVersion})
}

// Query wrappers

func sqliteBumpDataVersion(handler *Handler) (int64, error) {
	return handler.SqliteQuery_OneRow_Int64(queryKeyBumpDataVersion)
}

func sqliteDeleteItem(handler *Handler, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyDeleteItem, id)
}

func sqliteDeleteSection(handler *Handler, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyDeleteSection, id)
}

func sqliteDeleteStore(handler *Handler, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyDeleteStore, id)
}

func sqliteExistsItemById(handler *Handler, id int64) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyExistsItemById, id)
}

func sqliteExistsItemByName(handler *Handler, name string) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyExistsItemByName, name)
}

func sqliteExistsSectionByStoreIdSectionId(handler *Handler, store int64, section int64) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyExistsSectionByStoreIdSectionId, store, section)
}

func sqliteExistsStoreById(handler *Handler, id int64) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyExistsStoreById, id)
}

func sqliteExistsStoreByName(handler *Handler, name string) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyExistsStoreByName, name)
}

func sqliteGetDataVersion(handler *Handler) (int64, error) {
	return handler.SqliteQuery_OneRow_Int64(queryKeyGetDataVersion)
}

func sqliteGetSectionIdsByStore(handler *Handler, storeId int64) (*sql.Rows, error) {
	return handler.SqliteQuery_ManyRows(queryKeyGetSectionIdsByStore, storeId)
}

func sqliteInsertItem(handler *Handler, name string, onList bool) (int64, error) {
	return handler.SqliteQuery_OneRow_Int64(queryKeyInsertItem, name, onList)
}

func sqliteInsertSection(handler *Handler, store int64, name string) (int64, int64, error) {
	return handler.SqliteQuery_OneRow_Int64_Int64(queryKeyInsertSection, store, store, name)
}

func sqliteInsertStore(handler *Handler, name string) (int64, error) {
	return handler.SqliteQuery_OneRow_Int64(queryKeyInsertStore, name)
}

func sqliteItemOffList(handler *Handler, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyItemOffList, id)
}

func sqliteItemOnList(handler *Handler, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyItemOnList, id)
}

func sqliteItemStoreHasSection(handler *Handler, itemId int64, storeId int64) (bool, error) {
	return handler.SqliteQuery_OneRow_Bool(queryKeyItemStoreHasSection, itemId, storeId)
}

func sqliteUpdateItemName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyUpdateItemName, name, id)
}

func sqliteUpdateSectionName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyUpdateSectionName, name, id)
}

func sqliteUpdateSectionPosition(handler *Handler, position int64, id int64, store int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyUpdateSectionPosition, position, id, store)
}

func sqliteUpdateStoreName(handler *Handler, name string, id int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyUpdateStoreName, name, id)
}

func sqliteUpsertItemStore(handler *Handler, item int64, store int64, sold bool, section *int64) (sql.Result, error) {
	return handler.SqliteQuery_ZeroRows(queryKeyUpsertItemStore, item, store, sold, section)
}

// Crash-on-panic middleware

func crashOnPanicMiddleware(innerHandler http.Handler) http.Handler {
	handler := func(response http.ResponseWriter, request *http.Request) {
		defer func() {
			value := recover()
			if value != nil {
				slog.Error(
					"panic",
					"value", value,
				)
				os.Exit(1)
			}
		}()
		innerHandler.ServeHTTP(response, request)
	}
	return http.HandlerFunc(handler)
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
	db       *sql.DB
	logger   *slog.Logger
	request  *http.Request
	response http.ResponseWriter
	tx       *sql.Tx // The current transaction
}

func NewHandler(db *sql.DB, response http.ResponseWriter, request *http.Request) *Handler {
	return &Handler{
		db:       db,
		logger:   slog.Default(),
		request:  request,
		response: response,
		tx:       nil}
}

// Handler abstraction - request parsing

func (handler *Handler) DecodeJsonRequestBody(v any) bool {
	err := json.NewDecoder(handler.request.Body).Decode(v)

	if err != nil {
		http.Error(handler.response, err.Error(), http.StatusBadRequest)
		return true
	}

	return false
}

// Handler abstraction - response constructing and sending

func (handler *Handler) SendJsonResponse(statusCode int, v any) error {
	handler.response.Header().Set("Content-Type", "application/json")
	handler.response.WriteHeader(statusCode)
	return json.NewEncoder(handler.response).Encode(v)
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

// Handler abstraction - database helpers

func (handler *Handler) SqliteBeginTransaction() error {
	tx, err := handler.db.BeginTx(handler.request.Context(), nil)
	if err != nil {
		return err
	}
	handler.tx = tx
	return nil
}

func (handler *Handler) SqliteCommitTransaction() error {
	return handler.tx.Commit()
}

func (handler *Handler) SqliteRollbackTransaction() error {
	return handler.tx.Rollback()
}

func (handler *Handler) SqliteQuery_ZeroRows(key queryKey, args ...any) (sql.Result, error) {
	ctx := handler.request.Context()
	return handler.tx.StmtContext(ctx, preparedQueries[key]).ExecContext(ctx, args...)
}

func (handler *Handler) SqliteQuery_ZeroOrOneRows(key queryKey, args ...any) *sql.Row {
	ctx := handler.request.Context()
	return handler.tx.StmtContext(ctx, preparedQueries[key]).QueryRowContext(ctx, args...)
}

func (handler *Handler) SqliteQuery_ZeroOrOneRows_Int64(key queryKey, args ...any) (*int64, error) {
	row := handler.SqliteQuery_ZeroOrOneRows(key, args...)
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

func (handler *Handler) SqliteQuery_ZeroOrOneRows_String(key queryKey, args ...any) (*string, error) {
	row := handler.SqliteQuery_ZeroOrOneRows(key, args...)
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

func (handler *Handler) SqliteQuery_OneRow_Bool(key queryKey, args ...any) (bool, error) {
	row := handler.SqliteQuery_ZeroOrOneRows(key, args...)
	var x bool
	err := row.Scan(&x)
	return x, err
}

func (handler *Handler) SqliteQuery_OneRow_Int64(key queryKey, args ...any) (int64, error) {
	row := handler.SqliteQuery_ZeroOrOneRows(key, args...)
	var x int64
	err := row.Scan(&x)
	return x, err
}

func (handler *Handler) SqliteQuery_OneRow_Int64_Int64(key queryKey, args ...any) (int64, int64, error) {
	row := handler.SqliteQuery_ZeroOrOneRows(key, args...)
	var x int64
	var y int64
	err := row.Scan(&x, &y)
	return x, y, err
}

func (handler *Handler) SqliteQuery_ManyRows(key queryKey, args ...any) (*sql.Rows, error) {
	ctx := handler.request.Context()
	return handler.tx.StmtContext(ctx, preparedQueries[key]).QueryContext(ctx, args...)
}

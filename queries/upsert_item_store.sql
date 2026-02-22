INSERT INTO item_stores (item, store, sold, location)
SELECT ?, ?, ?, ?
ON CONFLICT (item, store) DO UPDATE
SET sold = excluded.sold, location = excluded.location

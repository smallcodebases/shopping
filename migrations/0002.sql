ALTER TABLE item_stores
ADD COLUMN in_store INTEGER NOT NULL DEFAULT 1
CHECK (in_store IN (0, 1) AND (in_store = 1 OR location IS NULL));

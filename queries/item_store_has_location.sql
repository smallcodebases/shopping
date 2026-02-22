SELECT EXISTS (
  SELECT 1
  FROM item_stores
  WHERE item = ?
    AND store = ?
    AND location IS NOT NULL
)

SELECT EXISTS (
  SELECT 1
  FROM items
  WHERE id = ?
)

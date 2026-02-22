SELECT EXISTS (
  SELECT 1
  FROM stores
  WHERE name = ?
)

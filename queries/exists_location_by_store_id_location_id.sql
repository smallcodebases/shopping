SELECT EXISTS (
  SELECT 1
  FROM locations
  WHERE store = ? AND id = ?
)


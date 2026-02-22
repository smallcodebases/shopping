INSERT INTO locations (store, position, name)
VALUES (
  ?,
  COALESCE(
    (SELECT MAX(position) + 1
      FROM locations
      WHERE store = ?
    ),
    0),
  ?
)
RETURNING id, position

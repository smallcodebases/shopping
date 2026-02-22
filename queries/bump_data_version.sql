UPDATE data_version
SET version = version + 1
RETURNING version

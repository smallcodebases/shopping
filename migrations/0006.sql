CREATE TABLE schema_version_2 (
    version INTEGER PRIMARY KEY
);

INSERT INTO schema_version_2 (version)
SELECT version
FROM schema_version;

DROP TABLE schema_version;

ALTER TABLE schema_version_2 RENAME TO schema_version;

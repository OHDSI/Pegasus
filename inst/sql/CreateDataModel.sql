DROP TABLE IF EXISTS @database_schema.phenotype;

CREATE TABLE @database_schema.phenotype (
  phenotype_id INT PRIMARY KEY,
  phenotype_name VARCHAR NOT NULL,
  clinical_definition VARCHAR NOT NULL,
  creator VARCHAR NOT NULL,
  time_created DATETIME NOT NULL
)



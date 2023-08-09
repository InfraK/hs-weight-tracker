CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) NOT NULL UNIQUE,
    created_at timestamptz default current_timestamp
);

INSERT INTO users (email)
VALUES ('santiagokent@gmail.com'),
    ('fake@email.com');

CREATE TABLE IF NOT EXISTS weights (
    id SERIAL PRIMARY KEY,
    weight_grams INTEGER NOT NULL,
    created_at timestamptz default current_timestamp
)
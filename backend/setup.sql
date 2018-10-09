DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

CREATE TABLE categories (
    id SERIAL PRIMARY KEY ,
    name text
);

CREATE TABLE cards (
    id SERIAL PRIMARY KEY,
    question text,
    answer text,
    last_answered_at timestamptz,
    due_at timestamptz,
    category_id int REFERENCES categories ON DELETE RESTRICT
);

CREATE INDEX cardscategories ON cards (category_id);

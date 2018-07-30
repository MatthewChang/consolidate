DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

CREATE TABLE categories (
    id SERIAL PRIMARY KEY ,
    name varchar(255)
);

CREATE TABLE cards (
    id SERIAL PRIMARY KEY,
    question varchar(255),
    answer varchar(255),
    last_correct_at timestamptz,
    due_at timestamptz,
    category_id int REFERENCES categories ON DELETE RESTRICT
);

CREATE INDEX cardscategories ON cards (category_id);

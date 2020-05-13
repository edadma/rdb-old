CREATE DATABASE st;

\connect st;

CREATE TABLE planet (
  plan_id SERIAL PRIMARY KEY,
  name TEXT,
  climate TEXT
);

CREATE TABLE species (
  spec_id SERIAL PRIMARY KEY,
  name TEXT,
  lifespan INTEGER,
  origin INTEGER NOT NULL REFERENCES planet(plan_id)
);

CREATE TABLE character (
  char_id SERIAL PRIMARY KEY,
  name TEXT,
  home INTEGER NOT NULL REFERENCES planet(plan_id),
  species INTEGER NOT NULL REFERENCES species(spec_id)
);

INSERT INTO planet (plan_id, name, climate) VALUES
  (1, 'Earth', 'not too bad'),
  (2, 'Turkana IV', null);

INSERT INTO species (spec_id, name, lifespan, origin) VALUES
  (1, 'Human', 71, 1);

INSERT INTO character (char_id, name, home, species) VALUES
  (1, 'Natasha Yar', 2, 1);

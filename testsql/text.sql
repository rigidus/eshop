CREATE TABLE products (
     product_id    SERIAL PRIMARY KEY,
     name          varchar(40) NOT NULL CHECK (name <> ''),
     price         money NOT NULL
);
CREATE TABLE p_groups (
     p_group_id    SERIAL PRIMARY KEY,
     name          varchar(40) NOT NULL CHECK (name <> '')
);


-- currval срабатывает ТОЛЬКО ПОСЛЕ первого инсёрта
-- http://postgresql.ru.net/manual/functions-sequence.html
SELECT currval('products_product_id_seq'::regclass);
SELECT setval('products_product_id_seq'::regclass, 16); -- вставка с 17 (16+1) прим.!!!


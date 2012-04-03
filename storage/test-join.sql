psql (9.1.3)
Type "help" for help.

ravtadb=> select * from product; select * from opname ; select * from option ;
 id |  name   
----+---------
  1 | oil
  2 | масло A
  3 | масло B
  4 | масло U
(4 rows)

 id |   name   
----+----------
  1 | объём
  2 | вязкость
  3 | вес
(3 rows)

 pr_id | op_id | value 
-------+-------+-------
     1 |     1 | 3л.
     1 |     1 | 4л.
     1 |     1 | 3,5л.
     1 |     1 | 4,5л.
     4 |     3 | 7,5г
(5 rows)

ravtadb=> select * from product, option ;
 id |  name   | pr_id | op_id | value 
----+---------+-------+-------+-------
  1 | oil     |     1 |     1 | 3л.
  1 | oil     |     1 |     1 | 4л.
  1 | oil     |     1 |     1 | 3,5л.
  1 | oil     |     1 |     1 | 4,5л.
  1 | oil     |     4 |     3 | 7,5г
  2 | масло A |     1 |     1 | 3л.
  2 | масло A |     1 |     1 | 4л.
  2 | масло A |     1 |     1 | 3,5л.
  2 | масло A |     1 |     1 | 4,5л.
  2 | масло A |     4 |     3 | 7,5г
  3 | масло B |     1 |     1 | 3л.
  3 | масло B |     1 |     1 | 4л.
  3 | масло B |     1 |     1 | 3,5л.
  3 | масло B |     1 |     1 | 4,5л.
  3 | масло B |     4 |     3 | 7,5г
  4 | масло U |     1 |     1 | 3л.
  4 | масло U |     1 |     1 | 4л.
  4 | масло U |     1 |     1 | 3,5л.
  4 | масло U |     1 |     1 | 4,5л.
  4 | масло U |     4 |     3 | 7,5г
(20 rows)

ravtadb=> select * from product, option where option.value = '3л.';
 id |  name   | pr_id | op_id | value 
----+---------+-------+-------+-------
  1 | oil     |     1 |     1 | 3л.
  2 | масло A |     1 |     1 | 3л.
  3 | масло B |     1 |     1 | 3л.
  4 | масло U |     1 |     1 | 3л.
(4 rows)

ravtadb=> select * from product, option where product.id = option.pr_id and option.value = '3л.';
 id | name | pr_id | op_id | value 
----+------+-------+-------+-------
  1 | oil  |     1 |     1 | 3л.
(1 row)

ravtadb=> select product.name, option.value from product, option where product.id = option.pr_id and option.value = '3л.';
 name | value 
------+-------
 oil  | 3л.
(1 row)


ravtadb=> 
ravtadb=> 
ravtadb=> 
ravtadb=> select product.name, opname.name, option.value
            from product, opname, option
            where product.id = option.pr_id
              and opname.id = option.op_id
              and option.value = '3л.';
 name | name  | value 
------+-------+-------
 oil  | объём | 3л.
(1 row)


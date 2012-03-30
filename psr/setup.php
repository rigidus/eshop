<?php
include ("config.php");
$query1 ="
CREATE TABLE IF NOT EXISTS `category` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `cat_name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  UNIQUE KEY `id` (`id`),
  KEY `cat_name` (`cat_name`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;";

$query2 ="
CREATE TABLE IF NOT EXISTS `hars` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` text COLLATE utf8_unicode_ci NOT NULL,
  `cat_id` int(11) NOT NULL,
  UNIQUE KEY `id` (`id`),
  KEY `cat_id` (`cat_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;";

$query3 ="
CREATE TABLE IF NOT EXISTS `hars_values` (
  `har_id` int(11) NOT NULL,
  `prod_id` int(11) NOT NULL,
  `har_value` longtext COLLATE utf8_unicode_ci NOT NULL,
  KEY `har_id` (`har_id`),
  KEY `prod_id` (`prod_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;";

$query4 ="
CREATE TABLE IF NOT EXISTS `images` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `prod_id` int(11) NOT NULL,
  `name` text COLLATE utf8_unicode_ci NOT NULL,
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;";

$query5 ="
CREATE TABLE IF NOT EXISTS `product` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `cat_id` int(11) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `image` text COLLATE utf8_unicode_ci NOT NULL,
  `price` text COLLATE utf8_unicode_ci NOT NULL,
  `brand` text COLLATE utf8_unicode_ci NOT NULL,
  `image_type` int(11) NOT NULL,
  `url` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  UNIQUE KEY `id` (`id`),
  KEY `cat_id` (`cat_id`),
  KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;";

$query6 ="
CREATE TABLE IF NOT EXISTS `pref` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `par` varchar(255) NOT NULL,
  `val` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci  AUTO_INCREMENT=1 ;";

$query7 ="
INSERT INTO `pref` (`id`, `par`, `val`) VALUES
(1, 'lang', 'russian'),
(2, 'antigate', '');";


@mysql_query($query1) or die("Error:".mysql_error()."");
@mysql_query($query2) or die("Error:".mysql_error()."");
@mysql_query($query3) or die("Error:".mysql_error()."");
@mysql_query($query4) or die("Error:".mysql_error()."");
@mysql_query($query5) or die("Error:".mysql_error()."");
@mysql_query($query6) or die("Error:".mysql_error()."");
@mysql_query($query7) or die("Error:".mysql_error()."");
echo "Installation finished";
?>
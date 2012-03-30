<?php
include ("config.php");
$query = "CREATE TABLE IF NOT EXISTS `pref` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `par` varchar(255) NOT NULL,
  `val` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci  AUTO_INCREMENT=1 ;" ;
$sort=mysql_query($query);

$query ="
INSERT INTO `pref` (`id`, `par`, `val`) VALUES
(1, 'lang', 'russian'),
(2, 'antigate', '');
";
$sort=mysql_query($query);
echo "Update finished<br />";
?>



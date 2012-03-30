<?php set_time_limit(0);error_reporting(E_ERROR | E_WARNING | E_PARSE); session_start();


// Коннект к Базе данных

$host = "localhost";          // хост
$database = "rus_parser";  // имя базы
$login = "root";              // логин
$password = "";               // пароль


#########sys.fun.#############
$connect = mysql_connect ($host, $login, $password) or die(mysql_error());
$select_db = mysql_select_db($database, $connect) or die(mysql_error());
mysql_query("set names cp1251");
$massparam='';$sql = mysql_query("SELECT * FROM `pref`"); while ($rowclubs = @mysql_fetch_array($sql)){$Config[$rowclubs['par']]=$rowclubs['val'];}if (!$Config['lang']) {$Config['lang']='russian';}$_=parse_ini_file('lang/'.$Config['lang'].'.ini');

?>
<link rel="stylesheet" type="text/css" href="styles.css" /> 
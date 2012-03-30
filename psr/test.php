<?php // This file is protected by copyright law and provided under license. Reverse engineering of this file is strictly prohibited.
$current_file=__FILE__;
$current_line=__LINE__;

$current_file="index.php";
$current_line=2;

$_88=88;

$fd_current_file=fopen($current_file,'rb');

while(--$current_line)
  fgets($fd_current_file,1024);

fgets($fd_current_file,4096);

/* Читает зашифорванную строчку из открытого файла длинной 372 байта */
$local_f_read = fread($fd_current_file,372);

/* Замена подстроки */
$replaced_substring = strtr($local_f_read,'EnteryouwkhRHYKNWOUTAaBbCcDdFfGgIiJjLlMmPpQqSsVvXxZz0123456789+/=','ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/');

/* $OO00O00O0 Зашифрованный контейнер */
$crypto_container=(base64_decode($replaced_substring));
/* Исполнение зашифрованного контейнера */
/* eval($crypto_container); */

/* Вторая часть расшифровщика */

$new_container=ereg_replace('index.php',"'".$OOO0O0O00."'",(base64_decode(strtr(fread($O000O0O00,$OO00O0000),'EnteryouwkhRHYKNWOUTAaBbCcDdFfGgIiJjLlMmPpQqSsVvXxZz0123456789+/=','ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'))));

echo ($new_container);

fclose($O000O0O00);
/* eval($new_container); */

phpinfo();

?>
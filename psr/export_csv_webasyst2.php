<?php
    require_once("header.php");
    echo "<b>".$_['exportforcsv']." WebAsyst</b><br />";
    $number = 0; // Количество товаров(случайных выбор из БД), если стоит 0, то выгружаются ВСЕ товары
    if($_POST['num'] != '') $number = $_POST['num'];
    function encodestring($st)
    {
        $st=strtr($st,"абвгдеёзийклмнопрстуфхъыэ_ ", "abvgdeeziyklmnoprstufhiiei-");
        $st=strtr($st,"АБВГДЕЁЗИЙКЛМНОПРСТУФХЪЫЭ_",  "ABVGDEEZIYKLMNOPRSTUFH'IEI");
        $st=strtr($st,array("ж"=>"zh", "ц"=>"ts", "ч"=>"ch", "ш"=>"sh", "щ"=>"shch","ь"=>"", "ю"=>"yu", "я"=>"ya", "Ж"=>"ZH", "Ц"=>"TS", "Ч"=>"CH", "Ш"=>"SH", "Щ"=>"SHCH","Ь"=>"", "Ю"=>"YU", "Я"=>"YA", "ї"=>"i", "Ї"=>"Yi", "є"=>"ie", "Є"=>"Ye" )  );
        $st = strtolower($st);
        return $st;
    }
    function jpgresize($in_file, $out_file, $larger)
    {
        $old_img=imagecreatefromjpeg($in_file);
        $old_img_size=getimagesize($in_file);
        if($old_img_size[0] >= $old_img_size[1])
        {
            $new_size_w = $larger;
            $new_size_h = $old_img_size[1]*$larger/$old_img_size[0];
        }
        else{
            $new_size_h = $larger;
            $new_size_w = $old_img_size[0]*$larger/$old_img_size[1];
        }
        $img_new=imagecreatetruecolor($new_size_w,$new_size_h);
        imagecopyresampled ($img_new, $old_img, 0, 0, 0, 0, $new_size_w, $new_size_h, $old_img_size[0], $old_img_size[1]);
        imageinterlace($img_new,1);
        imagejpeg($img_new,$out_file,100);
        imagedestroy($img_new); # убить объект, но не файл
        imagedestroy($old_img);
    }

    include('config.php');
    $q = mysql_query("select * from `category`") or die(mysql_error());
    $i = 0;
    while($q_r = mysql_fetch_assoc($q))
    {
        $ar[$i] = $q_r;
        $i++;
    }
    echo "<form action='' method=POST>
    ".$_['expcat'].":<br>
    <select class='textbox' name = 'cat'>";
    foreach($ar as $a)
    {
        $p = mysql_fetch_array(mysql_query("SELECT * FROM product WHERE cat_id='".$a['id']."' LIMIT 1"));
		$с = mysql_fetch_array(mysql_query("SELECT COUNT(*) AS cou FROM product WHERE cat_id='".$a['id']."'"));

        echo '<option value= "'.$a['id'].'">'.$a['cat_name'].' - '.$с['cou'].' шт.</option>';
    }
    echo "</select><br>".$_['expcol']."
    <input class='textbox' type='text' name='num' size=4 style='width:100px;'><br>".$_['exprand']."
    <input class='textbox' type='checkbox' name='rand'><br>
".$_['exph']." <input class='textbox' type='text' name='height' size=4 style='width:100px;' value='300'><br />
".$_['expwt']." <input class='textbox' type='text' name='height2' size=4 style='width:100px;' value='90'><br />
<br>".$_['exprewrite']." <input class='textbox' type='checkbox' name='synz'><br>
<br>Images Dir: <b>/</b><input class='textbox' type='text' name='dirz' size=4 style='width:100px;'><b>/</b><br>
    <input class='button' type = 'submit' name='exp' value = '".$_['expgo']."'>
    </form>";
    if($_POST['exp'])
    {
        $q = mysql_query("select * from `category` where `id` = '".$_POST['cat']."'") or die(mysql_error());
        while($q_r = mysql_fetch_assoc($q))
        {
            $cat_id = $q_r['id'];
            $cat_name = $q_r['cat_name'];
        }
        $cat_url = encodestring($cat_name);
$cat_name=preg_replace('@.*?: @smi','',$cat_name);
$cat_url=preg_replace('@.*?:-@smi','',$cat_url);
        $dir = "export/".encodestring($cat_name).'_'.date('d-m-y'); 
        $fname = $dir."/".$cat_url.'_'.date('d-m-y').'.csv';
		@mkdir($dir);
		$tmpz=explode('/',$_POST['dirz']);
		$full='';
		foreach ($tmpz as $n)
		{
		$full.='/'.$n;
		@mkdir($dir.$full);
		}
		$wr=substr($full,1);

        //@mkdir($dir."/shop/");
        //@mkdir($dir."/shop/images/");
        #@mkdir($dir."/resized/");
        @chmod($dir, 0777);
        $to = $to."Артикул;Наименование (Русский);ID страницы (часть URL, используется в ссылках на эту страницу);Цена;Название вида налогов;Скрытый;Можно купить;Старая цена;На складе;Продано;Описание (Русский);Краткое описание (Русский);Сортировка;Заголовок страницы (Русский);Тэг META keywords (Русский);Тэг META description (Русский);Стоимость упаковки;Вес продукта;Бесплатная доставка;Ограничение на минимальный заказ продукта (штук);Файл продукта;Количество дней для скачивания;Количество загрузок (раз);Фотография;Фотография;\n
        ;".$cat_name.";".encodestring($cat_name).";\n";
        $f = fopen($fname, 'a+');
        fwrite($f, $to);
        fclose($f);
        $q = mysql_query("select * from `product` where `cat_id` = '".$cat_id."'") or die(mysql_error());
        $i = 0;
        while($q_r = mysql_fetch_assoc($q))
        {
            $pr[$i] = $q_r;
            $i++;
        }
        if($_POST['rand'] == 'on') shuffle($pr);
        //print_r($pr);die();
        $j = rand(0,99)+time();
        foreach($pr as $p)
        {
            $q = mysql_query("select * from `hars_values` where `prod_id` = '".$p['id']."'") or die(mysql_error());
            $ar = array();
            $i = 0;
            unset($ar);
            while($q_r = mysql_fetch_assoc($q))
            {
                $ar[$i] = $q_r;
                $i++;
            }
            $p['descr'] = '<table><tr><td>Характеристика</td><td>Значение</td>';
            $p['brief'] = '<ul>';
            $i = 0;
            $hnames = array();
            foreach($ar as $a)
            {
                $q = mysql_query("select `name` from `hars` where `id` = '".$a['har_id']."'") or die(mysql_error());
                while($q_r = mysql_fetch_assoc($q)) $har_name = $q_r['name'];
                if(!in_array($har_name, $hnames))
                {
                    $p['descr'] = $p['descr'].'<tr><td>'.$har_name.'</td><td>'.$a['har_value'].'</td></tr>';
                    if($i < 8) $p['brief'] = $p['brief'].'<li><b>'.$har_name.'</b> : '.$a['har_value'].'</li>';
                    $hnames[$i] = $har_name;
                    $i++;
                }
            }
            $p['brief'] = $p['brief'].'</ul>';
            $p['descr'] = $p['descr'].'</table>';
            $p ['art'] = $j;

            //$p['price'] = round($p['price']*3.75,0);

            foreach ($p as $key=>$value)
            {
                $p[$key]=str_replace(";", ",", $p[$key]);
                $p[$key]=str_replace("\n", ",", $p[$key]);
                $p[$key]=str_replace("\r", ",", $p[$key]);
            }
            unset($_price);
            for($n=0;$n<strlen($p['price']);$n++) {
                if(is_numeric($p['price'][$n])) {
                $_price .= $p['price'][$n];
                }
            }
			if ($_POST['synz']) {$p['brief']=syn($p['brief']);$p['descr']=syn($p['descr']);}
            $to = $p['art'].";".$p['name'].";".$j.";".$_price.";;0;1;0;100;0;".$p['descr'].";".$p['brief'].";0;".$p['name'].";".$p['name'].";".$p['name']."; 0;0;0;1;;0;0;".$wr."/midl_".$p['image'].",".$wr."/".str_replace('.jpg','_thm.jpg',$p['image']).",".$wr."/".$p['image'].";;\n";
            
            $f = fopen($fname, 'a+');
            fwrite($f, $to);
            fclose($f);
            if(file_exists("images/".$p['image']) && filesize("images/".$p['image'])>0) {
				copy("images/".$p['image'],$dir.$full.'/'.$p['image']);
                jpgresize("images/".$p['image'], $dir.$full."/midl_".$p['image'], $_POST['height']);
                jpgresize("images/".$p['image'], $dir.$full.'/'.str_replace('.jpg','_thm.jpg',$p['image']), $_POST['height2']);
            }
            if($number != 0 and $j == $number) die("".$_['expend']." <a href='".$fname."'>".$fname."</a>");
            $j++;

        }
        echo "".$_['expend']." <a href='".$fname."'>".$fname."</a>";
    }
    require_once("footer.php");
?>
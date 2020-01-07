<?php

function get_lang_defs() {
    $arr = array();
    $arr['en'] = "English";
    $arr['ru'] = "Russian";
    $arr['ua'] = "Ukrainian";
    $arr['pl'] = "Polish";
    $arr['fr'] = "French";
    $arr['de'] = "German";
    $arr['it'] = "Italian";
    $arr['es'] = "Spanish";
    $arr['nl'] = "Netherlandish";
    $arr['lt'] = "Lithuanian";
    $arr['ee'] = "Estonian";
    return $arr;
}

function get_lang_complete() {
    global $link;

    $q = "SELECT `lang_values`.`lang`, `lang_values`.`key_id`
    FROM `lang_values`, `lang_keys`
    where `lang_values`.`key_id` = `lang_keys`.`id` and `lang_values`.`status` = 1 order by `key_id`";
    $res = mysqli_query($link, $q);

    $q2 = "select * from `lang_keys` ";
    $res2 = mysqli_query($link, $q2);
    $total = mysqli_num_rows($res2);

    $arr = get_lang_defs();
    $res_array = array();
    foreach ($arr as $index => $val) {
        $res_array[$index] = 0;
    }

    while ($row = mysqli_fetch_array($res)) {
        $res_array[$row['lang']]++;
    }

    foreach ($arr as $index => $val) {
        $res_array[$index] = round(($res_array[$index] / $total) * 100);
    }
    return $res_array;

//
//    $q = "select * from `lang_sections` order by `id`";
//    $res = mysqli_query($link, $q);
//    while ($row = mysqli_fetch_array($res)) {
//        $q2 = "select * from `lang_keys` where `section_id`='".$row['id']."' order by `id`";
//        $res2 = mysqli_query($link, $q2);
//        while ($row2 = mysqli_fetch_array($res2)) {
//            $total++;
//            if (get_key_default_value($row2['id'], $lang)) {
//                $translated++;
//            }
//        }
//    }
//    return round(($translated / $total) * 100);
}

function print_header() {
    $access = 'none';
    switch ($_SESSION['access']) {
        case 0 : $access = 'none'; break;
        case 1 : $access = 'commiter'; break;
        case 10 : $access = 'admin'; break;
    }
    echo 'logged as : <b><u>'.$_SESSION['user'].'</u></b> status: '.$access.'<br>';

    echo '<a href="lang.php">languages and translate</a><br>';
    if ($_SESSION['access'] >= 10) echo '<a href="struct.php">structure (admin)</a><br> ';
    if ($_SESSION['access'] >= 10) echo '<a href="export.php">export (admin)</a><br> ';
}


function get_section($id) {
    global $link;
    $q = "select * from `lang_sections` where `id` = '".$id."'";
    $res = mysqli_query($link, $q);
    if ($row = mysqli_fetch_array($res)) return $row;
    else return false;
}
function get_key($id) {
    global $link;
    $q = "select * from `lang_keys` where `id` = '".$id."'";
    $res = mysqli_query($link, $q);
    if ($row = mysqli_fetch_array($res)) return $row;
    else return false;
}
function get_value($id) {
    global $link;
    $q = "select * from `lang_values` where `id` = '".$id."'";
    $res = mysqli_query($link, $q);
    if ($row = mysqli_fetch_array($res)) return $row;
    else return false;
}
function get_key_lang($id, $lang) {
    global $link;
    $q = "select `account`.`login`, `lang_values`.`id` as 'vid', `lang_values`.`value`, `lang_values`.`status`, `lang_values`.`approved` from `lang_values` left join `account` on `account`.`id` = `lang_values`.`user_id` where `key_id` = '".$id."' and `lang`='".$lang."'";
    $res = mysqli_query($link, $q);
    if (mysqli_num_rows($res) > 0) {
        $r = array();
        while ($row = mysqli_fetch_array($res)) {
            $r[] = $row;
        }
        return $r;
    } else {
        return false;
    }
}
function get_key_default_value($id, $lang) {
    global $link;
    $q = "select * from `lang_values` where `key_id` = '".$id."' and `status`=1 and `lang`='".$lang."'";
    $res = mysqli_query($link, $q);
    if ($row = mysqli_fetch_array($res)) return $row;
    else return false;
}
function get_account($id) {
    global $link;
    $q = "select * from `account` where `id` = '".$id."'";
    $res = mysqli_query($link, $q);
    if ($row = mysqli_fetch_array($res)) return $row;
    else return false;
}

function export_lang($lang) {
    global $link;
    $s = '; '.$lang."\n";

    $q = "select * from `lang_sections` order by `id`";
    $res = mysqli_query($link, $q);
    while ($row = mysqli_fetch_array($res)) {
        $s .= '['.$row['name']."]\n";

        $q2 = "SELECT * FROM `lang_keys`
        left join `lang_values` on `lang_values`.`key_id` = `lang_keys`.`id`
        where `lang_values`.`status` = 1
        and `lang_values`.`lang` = '".$lang."'
        and `lang_keys`.`section_id` = ".$row['id'];

        $res2 = mysqli_query($link, $q2);
        while($row2 = mysqli_fetch_array($res2)) {
            $s .= $row2['name'].'='.$row2['value']."\n";
        }
    }

    if ($fp = fopen('lang_'.$lang.'.txt', 'w')) {
        fwrite($fp, $s);
        fclose($fp);
        echo 'file updated';
    } else {
        echo 'fail create file: lang_'.$lang.'.txt';
    }

}
?>
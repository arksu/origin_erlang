<?php
require_once('../config.php');
require_once('utils.php');
session_start ();

if (isset($_SESSION['user'])) {
    $link = mysqli_connect( $cfg['db_host'], $cfg['db_user'], $cfg['db_pass'], $cfg['db_name'] );
    mysqli_set_charset($link, "utf8");
    mysqli_query($link, "SET NAMES utf8");
    mysqli_query($link, "SET CHARSET utf8");

    print_header();
    if (isset($_GET['action'])) {
        switch ($_GET['action']) {
            case 'export' : export_lang($_GET['lang']); break;


            default : show_all_langs();
        }
    } else {
        show_all_langs();
    }

} else {
    header("Request-URI: login.php");
    header("Content-Location: login.php");
    header("Location: login.php");
}

function show_all_langs() {
    $arr = get_lang_defs();
    foreach($arr as $index => $val) {
        echo '<a href="?action=export&lang='.$index.'">'.$val.'</a><br>';
    }
}


?>
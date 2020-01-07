<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
</head>
<body>
<?php
require_once('../config.php');
require_once('utils.php');

session_start ();

if (isset($_SESSION['user'])) {
    $link = mysqli_connect( $cfg['db_host'], $cfg['db_user'], $cfg['db_pass'], $cfg['db_name'] );
    mysqli_set_charset($link, "utf8");
    $access_level = $_SESSION['access'];



    if (isset($_SESSION['lang']))
        $lang = $_SESSION['lang'];
    else
        $lang = "en";

    print_header();
    print_langs();

    if (isset($_GET['action'])) {
        switch ($_GET['action']) {
            case 'add_value' : add_value(); break;
            case 'approve' : approve(); show_all_values(); break;
            case 'set_lang' : set_lang(); show_all_values(); break;

            default : show_all_values();
        }
    } else if (isset($_POST['action'])) {
        switch ($_POST['action']) {
            case 'add_value_post' : add_value_post(); show_all_values(); break;

            default : show_all_values();
        }
    } else {
        show_all_values();
    }

} else {
    header("Request-URI: login.php");
    header("Content-Location: login.php");
    header("Location: login.php");
}
function approve() {
    global $link;
    global $lang;
    global $access_level;
    if ($access_level <= 0) return;

    $key_id = mysqli_real_escape_string($link, $_GET['key_id']);
    $id = mysqli_real_escape_string($link, $_GET['id']);
    $q = "update `lang_values` set `status` = 0 where `key_id` = '".$key_id."' and `lang` = '".$lang."'";
    mysqli_query($link, $q);

    $q = "update `lang_values` set `status` = 1, `approved`=".$_SESSION['user_id']." where `id` = '".$id."' and `lang` = '".$lang."'";
    mysqli_query($link, $q);

    export_lang($lang);

}
function add_value() {
    global $link;
    global $lang;
    global $access_level;
    $key_id = mysqli_real_escape_string($link, $_GET['key_id']);
    $k = get_key($key_id);
    echo 'Lang: '.$lang.'<br>';
    echo '<h1>'.$k['name'].'</h1><br>';
    echo '
        <form method="post" action="'.$_SERVER['SCRIPT_NAME'].'">
            <table border="0"><tr>
                <td>Value:</td><td><input name="name"></td><tr>
            </table>
            <input name="action" type="hidden" value="add_value_post">
            <input name="key_id" type="hidden" value="'.$key_id.'">
            <input type="submit" value="ok"><br>
        </form><br>';

    echo 'user variants:<br>';
    if ($vals = get_key_lang($key_id, $lang)) {
        echo '<table border="0">';
        foreach ($vals as $v) {
            echo '<tr>';
            if ($access_level > 0)
            echo '<td><b><u>'.$v['login'].'</u></b></td><td><a href="?action=approve&key_id='.$key_id.'&id='.$v['vid'].'">approve</a></td><td> '.$v['value'].'</td>';
            else
                echo '<td><b><u>'.$v['login'].'</u></b></td><td>: '.$v['value'].'</td>';

            if ($v['status'] == 1) {
                if ($acc = get_account($v['approved'])) {
                    $approve_name = $acc['login'];
                } else $approve_name = 'unknown';

                echo '<td><font color="#7cfc00">approved ('.$approve_name.')</font></td>';
            }

            echo '</tr>';
        }
        echo '</table>';


    } else echo 'empty<br>';
}
function add_value_post() {
    global $link;
    $name = mysqli_real_escape_string($link, $_POST['name']);
    $key_id = mysqli_real_escape_string($link, $_POST['key_id']);
    $user_id = mysqli_real_escape_string($link, $_SESSION['user_id']);
    global $lang;
    $q = "insert into `lang_values` (`id`,`value`,`key_id`,`lang`, `user_id`) values (NULL, '".$name."', '".$key_id."', '".$lang."', '".$user_id."')";
    mysqli_query($link, $q);
}

function print_langs() {
    echo 'Languages: ';
    $arr = get_lang_defs();
    $perc = get_lang_complete($index);
    foreach ($arr as $index => $val) {
        $p = $perc[$index];
        echo '<a href="?action=set_lang&lang='.$index.'">';
        if ($p < 10) echo '<font color="red">';
        echo $val;
        echo ' ('.$p.'%)';
        if ($p < 10) echo '</font>';
        echo '</a>  ';
    }
    echo '<br><br>';
}
function set_lang() {
    global $link;
    global $lang;
    $lang = mysqli_real_escape_string($link, $_GET['lang']);
    $_SESSION['lang'] = $lang;
}

function show_all_values() {
    global $link;
    global $lang;
    echo '<h1><u>Language: '.$lang.'</u></h1>';
    // show all sections
    $q = "select * from `lang_sections` order by `id`";
    $res = mysqli_query($link, $q);
    echo '<table border="0">';
    while ($row = mysqli_fetch_array($res)) {
        echo '<tr><td colspan="10">';
        echo '<h2>'.$row['name'].'</h2>';
        echo '</td></tr>';
        show_keys($row['id']);
    }
    echo '</table>';

}
function show_keys($section_id) {
    global $link;
    global $lang;
    global $access_level;
    $q = "select * from `lang_keys` where `section_id`='".$section_id."' order by `id`";
    $res = mysqli_query($link, $q);
    while ($row = mysqli_fetch_array($res)) {
        echo '<tr><td width="100"></td><td>';
        echo $row['name'];
        echo '</td><td>';
        $def = false;
        if ($val = get_key_default_value($row['id'], $lang)) {
            echo '<a href="?action=add_value&key_id='.$row['id'].'">'.$val['value'].'</a>';
            $def = true;
        }
        else
            echo '<a href="?action=add_value&key_id='.$row['id'].'"><font color="red">___ empty ___</font></a>';
        if (!$def && $val = get_key_lang($row['id'], $lang)) {
            echo ' << need approve';
        }
        echo '</td></td></tr>';
    }
}
?>
</body>
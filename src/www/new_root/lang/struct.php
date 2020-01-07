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

    print_header();

    if (isset($_GET['action'])) {
        switch ($_GET['action']) {
            case 'add_section' : add_section(); break;
            case 'edit_section' : edit_section(); break;
            case 'delete_section' : delete_section(); show_all_values(); break;

            case 'add_key' : add_key(); break;
            case 'edit_key' : edit_key(); break;
            case 'delete_key' : delete_key(); show_all_values(); break;

            default : show_all_values();
        }
    } else if (isset($_POST['action'])) {
        switch ($_POST['action']) {
            case 'add_section_post' : add_section_post(); show_all_values(); break;
            case 'edit_section_post' : edit_section_post(); show_all_values(); break;

            case 'add_key_post' : add_key_post(); show_all_values(); break;
            case 'edit_key_post' : edit_key_post(); show_all_values(); break;

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

function add_section_post() {
    global $link;
    $name = mysqli_real_escape_string($link, $_POST['name']);
    $q = "insert into `lang_sections` (`id`,`name`) values (NULL, '".$name."')";
    mysqli_query($link, $q);
}

function add_section() {
    echo '
        <form method="post" action="'.$_SERVER['SCRIPT_NAME'].'">
            <table border="0"><tr>
                <td>Name:</td><td><input name="name"></td><tr>
            </table>
            <input name="action" type="hidden" value="add_section_post">
            <input type="submit" value="ok"><br>
        </form>';
}
function edit_section_post() {
    global $link;
    $name = mysqli_real_escape_string($link, $_POST['name']);
    $id = mysqli_real_escape_string($link, $_POST['id']);
    $q = "update `lang_sections` set `name`='".$name."' where `id`='".$id."'";
    mysqli_query($link, $q);
}
function edit_section() {
    global $link;
    $id = mysqli_real_escape_string($link, $_GET['id']);
    $section = get_section($id);
    echo '
        <form method="post" action="'.$_SERVER['SCRIPT_NAME'].'">
            <table border="0"><tr>
                <td>Name:</td><td><input name="name" value="'.$section['name'].'"></td><tr>
            </table>
            <input name="action" type="hidden" value="edit_section_post">
            <input name="id" type="hidden" value="'.$id.'">
            <input type="submit" value="ok"><br>
        </form>';

    echo '<a href="?action=delete_section&id='.$id.'">delete</a>';
}
function delete_section() {
    global $link;
    $id = mysqli_real_escape_string($link, $_GET['id']);
    $q = "delete from `lang_sections` where `id`='".$id."'";
    mysqli_query($link, $q);
}



function add_key_post() {
    global $link;
    $name = mysqli_real_escape_string($link, $_POST['name']);
    $section_id = mysqli_real_escape_string($link, $_POST['section_id']);
    $q = "insert into `lang_keys` (`id`,`name`,`section_id`) values (NULL, '".$name."', '".$section_id."')";
    mysqli_query($link, $q);
}

function add_key() {
    global $link;
    $section_id = mysqli_real_escape_string($link, $_GET['section_id']);
    $section = get_section($section_id);
    echo '<h1>'.$section['name'].'</h1>';
    echo '
        <form method="post" action="'.$_SERVER['SCRIPT_NAME'].'">
            <table border="0"><tr>
                <td>Name:</td><td><input name="name"></td><tr>
            </table>
            <input name="action" type="hidden" value="add_key_post">
            <input name="section_id" type="hidden" value="'.$section_id.'">
            <input type="submit" value="ok"><br>
        </form>';
}
function edit_key_post() {
    global $link;
    $name = mysqli_real_escape_string($link, $_POST['name']);
    $id = mysqli_real_escape_string($link, $_POST['id']);
    $q = "update `lang_keys` set `name`='".$name."' where `id`='".$id."'";
    mysqli_query($link, $q);
}
function edit_key() {
    global $link;
    $id = mysqli_real_escape_string($link, $_GET['id']);
    $key = get_key($id);
    echo '
        <form method="post" action="'.$_SERVER['SCRIPT_NAME'].'">
            <table border="0"><tr>
                <td>Name:</td><td><input name="name" value="'.$key['name'].'"></td><tr>
            </table>
            <input name="action" type="hidden" value="edit_key_post">
            <input name="id" type="hidden" value="'.$id.'">
            <input type="submit" value="ok"><br>
        </form>';

    echo '<a href="?action=delete_key&id='.$id.'">delete</a>';
}
function delete_key() {
    global $link;
    $id = mysqli_real_escape_string($link, $_GET['id']);
    $q = "delete from `lang_keys` where `id`='".$id."'";
    mysqli_query($link, $q);
}



function show_all_values() {
    global $link;
    // show all sections
    $q = "select * from `lang_sections` order by `id`";
    $res = mysqli_query($link, $q);
    while ($row = mysqli_fetch_array($res)) {
        echo '<h2><a href="?action=edit_section&id='.$row['id'].'">'.$row['name'].'</a></h2>';
        show_keys($row['id']);
    }

    echo '<a href="?action=add_section">add new section</a><br>';
}

function show_keys($section_id) {
    global $link;
    $q = "select * from `lang_keys` where `section_id`='".$section_id."' order by `id`";
    $res = mysqli_query($link, $q);
    echo '<table border="0">';
    while ($row = mysqli_fetch_array($res)) {
        echo '<tr><td width="100"></td><td>';
        echo '<a href="?action=edit_key&id='.$row['id'].'">'.$row['name'].'</a>';
        echo '</td></tr>';
        //show_keys($row['id']);
    }
    echo '<tr><td width="100"></td><td>';
    echo '<a href="?action=add_key&section_id='.$section_id.'">add new key</a><br>';
    echo '</td></tr>';
    echo '</table>';
}









?>
</body>
<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
</head>
<body>
<?php
require_once('../config.php');

session_start ();

if (isset($_SESSION['user'])) {
    if ($_GET['action'] == "logout") {
        if (session_destroy())
            echo 'success logged out';
        else
            echo 'error!';

    } else {
        echo 'already logged! <br> <a href="lang.php">go work</a> <br><a href="?action=logout">logout</a>';
    }

} else {

    if (isset($_POST['user']) && isset($_POST['pass'])) {
        $user = $_POST['user'];
        $pass = $_POST['pass'];

        $link = mysqli_connect( $cfg['db_host'], $cfg['db_user'], $cfg['db_pass'], $cfg['db_name'] );
        mysqli_set_charset($link, "utf8");
        print(mysqli_character_set_name($link));
        $q = "select `lang_status`,`id` from account where `login`='".mysqli_real_escape_string($link, $user)."' and `pass`='".mysqli_real_escape_string($link, $pass)."' limit 1";
        $res = mysqli_query($link, $q);
        if ($row = mysqli_fetch_array($res)) {
            $_SESSION['access'] = $row['lang_status'];
            $_SESSION['user'] = mysqli_real_escape_string($link, $user);
            $_SESSION['pass'] = mysqli_real_escape_string($link, $pass);
            $_SESSION['user_id'] = mysqli_real_escape_string($link, $row['id']);
            header("Request-URI: lang.php");
            header("Content-Location: lang.php");
            header("Location: lang.php");
        } else {
            echo 'auth error!';
        }
    } else {
        echo '
        <html>
        <title>Login transtale</title>
        <form method="post">
            <table border="0"><tr>
                <td>Name:</td><td><input name="user"></td><tr>
                <td>Password:</td><td><input name="pass" type="password"></td>
            </table>
            <input type="submit" value="ok">
        </form>

        ';
    }
}


?>
</body>

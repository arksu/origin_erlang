<?php
//print_r($_POST);
?>
<html>

    <head>
        <title>Regga</title>
        <link rel="stylesheet" href="forms.css">
        <script type="text/javascript" src="js/jquery.js"></script>

<?php
        if (isset($_POST['act']) && $_POST['act'] == 'pass') {
?>
            <script type="text/javascript" src="js/jquery.showpassword.min.js"></script>


    <script type="text/javascript">

                $().ready(function() {
            $("#id_password").showPassword('#checker', {
                        text: '<a class="dotted">Show password</a>'
            });

        });
    </script>
<?php
        }
?>
        <script type="text/javascript">

            var _gaq = _gaq || [];
            _gaq.push(['_setAccount', 'UA-22592514-2']);
            _gaq.push(['_trackPageview']);

            (function() {
                var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
            })();

        </script>
    </head>
    <body>
    <div class="main_reg">

        <div class="section_container">
            <h2>Registration</h2>

<?php
require_once 'db_work.php';
require_once 'utils.php';

// phpbb func -------------------------------------------------------------------------
define('IN_PHPBB', true);
$phpbb_root_path = (defined('PHPBB_ROOT_PATH')) ? PHPBB_ROOT_PATH : './forum/';
$phpEx = substr(strrchr(__FILE__, '.'), 1);
include($phpbb_root_path . 'common.' . $phpEx);
include($phpbb_root_path . 'includes/functions_user.' . $phpEx);

//-------------------------------------------------------------------------------------
function add_user_to_forum($user, $pass, $email) {


    // User's data
    $user_row['username'] = $user;
    $user_row['user_password'] = phpbb_hash($pass);
    $user_row['user_email'] = $email;
    $user_row['group_id'] = 2;
    $user_row['user_type'] = 0;
    // Adding user
    user_add($user_row, false);

    //echo 'done! user='.$user.' pwd='.$pass.' mail='.$email;
}
//-------------------------------------------------------------------------------------

if (isset($_POST['act'])) {
    switch ($_POST['act']) {
        case 'login' :
            show_form_login(); break;
        case 'pass' :
            show_form_pass(); break;
        case 'reg' :
            reg_user(); break;
        case 'email' :
            show_form_email(); break;
        default :
        show_form_login();
    }
} else {
    show_form_login();
}

function show_form_login() {
    ?>
    <p>Please, introduce yourself</p>

    <form action="<?php echo $_SERVER['SCRIPT_NAME']; ?>" method="post">
    <input name="act" value="email" type="hidden">
        <div class="signup_form_section">
            <p><input type="text" class="required" name="login" maxlength="32" placeholder="Nickname"></p>
            <p><button type="submit" style="display: inline-block; ">Continue</button></p>


            <div id="submitting" class="no_align" style="display: none; ">
                <p><img src="images/spinner.gif">Validating nickname...</p>
            </div>

        </div>
    </form>
    <?php
}

function show_form_email() {
    if (!isset($_POST['login'])) die ("error login");
    $login = $_POST['login'];

    if (!check_param($login)) {
        ?>
            <p>Incorrect nickname!</p>
            <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                <input name="email" value="<?php echo $_POST['email']; ?>" type="hidden">
                <input name="act" value="login" type="hidden">
                <p><button type="submit" style="display: inline-block; ">Try again</button></p>
            </form>
        <?php

    } else {

    db_connect();

    if (check_exist_account($login)) {

        if (check_exist_char($login)) {
            ?>
                <p>Enter your e-mail address</p>

                <form action="<?php echo $_SERVER['SCRIPT_NAME']; ?>" method="post">
                <input name="act" value="pass" type="hidden">
                <input name="login" value="<?php echo $login; ?>" type="hidden">
                <div class="signup_form_section">
                        <p><input id="id_email" type="text" class="required" name="email" maxlength="75" placeholder="Email address"></p>
                                <p><button type="submit" style="display: inline-block; ">Continue</button></p>


                <div id="submitting" class="no_align" style="display: none; ">
                    <p><img src="images/spinner.gif">Validating email...</p>
                </div>

                </div>
                </form>
            <?php
        } else {
            // exist char
            ?>
                <p>Sorry, this nickname already exist</p>

                <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                    <input name="act" value="login" type="hidden">
                    <p><button type="submit" style="display: inline-block; ">Try again</button></p>
                </form>
            <?php
        }
    } else {
        // exist account
        ?>
                <p>Sorry, this account already exist</p>

                <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                    <input name="act" value="login" type="hidden">
                    <p><button type="submit" style="display: inline-block; ">Try again</button></p>
                </form>

                <br>
                <p>Forget your password?</p>

                <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                    <input name="login" value="<?php echo $login; ?>" type="hidden">
                    <input name="act" value="reset" type="hidden">
                    <p><button type="submit" style="display: inline-block; ">Reset password</button></p>
                </form>

        <?php
    }
    }

}

function show_form_pass() {
    if (!isset($_POST['email'])) die ("error email");
    $email = $_POST['email'];
    if (!isset($_POST['login'])) die ("error login");
    $login = $_POST['login'];

    if (preg_match('|^([a-z0-9_\.\-]{1,20})@([a-z0-9\.\-]{1,20})\.([a-z]{2,4})|is', $email)) {
        db_connect();
        if (check_exist_email($email)) {
           ?>
               <p>Choose your password</p>

               <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                   <input name="email" value="<?php echo $_POST['email']; ?>" type="hidden">
                   <input name="login" value="<?php echo $_POST['login']; ?>" type="hidden">
                   <input name="act" value="reg" type="hidden">
                   <div class="signup_form_section">
                       <p class="checker">
                           <input id="id_password" type="password" class="validate[required,minSize[5]]" name="pass" placeholder="Password"/>
                       </p>
                       <p class="checker2"><span id="checker"></span></p>


                       <p><button type="submit" style="display: inline-block; ">Continue</button></p>

                       <div id="submitting" class="no_align" style="display: none; ">
                           <p><img src="images/spinner.gif">Register...</p>
                       </div>

                   </div>
               </form>
           <?php

       } else {
           ?>
               <p>Sorry, this email is already in use</p>

               <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                   <input name="act" value="email" type="hidden">
                   <input name="login" value="<?php echo $login; ?>" type="hidden">
                   <p><button type="submit" style="display: inline-block; ">Enter another email</button></p>
               </form>
           <?php
       }
    } else {
        ?>
            <p>Incorrect e-mail!</p>
            <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                <input name="act" value="email" type="hidden">
                <input name="login" value="<?php echo $login; ?>" type="hidden">
                <p><button type="submit" style="display: inline-block; ">Enter another email</button></p>
            </form>
        <?php

    }

}


function reg_user() {
    if (!isset($_POST['email'])) die ("error email");
    $email = $_POST['email'];
    if (!isset($_POST['login'])) die ("error login");
    $login = $_POST['login'];
    if (!isset($_POST['pass'])) die ("error pass");
    $pass = $_POST['pass'];

    $reg_success = false;
    $reg_msg = '';

    if (!preg_match('|^([a-z0-9_\.\-]{1,20})@([a-z0-9\.\-]{1,20})\.([a-z]{2,4})|is', $email)) $reg_msg = "wrong email";
    if (!check_param($login)) $reg_msg ="incorrect login! check symbols";
    if (!check_param($pass)) $reg_msg = "incorrect pass! check symbols";

    db_connect();

    if (!check_exist_account($login)) $reg_msg = 'login already exist';
    if (!check_exist_email($email)) $reg_msg = 'email already exist';
    if (!check_exist_char($login)) $reg_msg = 'This char already exist';

    if (strlen($reg_msg) == 0) {
        // добавляем юзера
        $reg_id = md5($login.$pass);
        $q = 'INSERT INTO `account` (`login`,`pass`,`email`,`reg_id`,`status`) VALUES ("'.$login.'","'.$pass.'","'.$email.'","'.$reg_id.'",0)';
        db_query($q);

        $q = 'SELECT `id` FROM `account` WHERE `reg_id`="'.$reg_id.'"';
        $res_acc = db_query($q);

        if  ($row = db_fetch_array($res_acc)) {

            // добавляем чара игроку
            $q = 'INSERT INTO `char` (`account_id`,`name`,`x`,`y`) VALUES ("'.$row['id'].'", "'.$login.'", "3000", "3000")';
            db_query($q);

            // создаем юзера на форуме
            add_user_to_forum($login, $pass, $email);

            $reg_success = true;
            $reg_msg = 'success';
        } else {
            $reg_msg = 'none account id';
        }
    }

    if ($reg_success) {
        echo '
        <p>Thank you for registration!</p>
        <p>Now you can begin <a href="download.html">playing</a></p>
        ';
    } else {
        ?>
            <p><?php echo $reg_msg; ?>!</p>
            <form method="post" action="<?php echo $_SERVER['SCRIPT_NAME']; ?>">
                <p><button type="submit" style="display: inline-block; ">Try again</button></p>
            </form>
        <?php
    }

}

function check_exist_account($s) {
    $q = 'SELECT `id` FROM `account` WHERE `login`="'.$s.'"';
    $res = db_query($q);
    if (db_num_rows($res) <> 0) {
        return false;
    }
    return true;
}
function check_exist_email($s) {
    $q = 'SELECT `id` FROM `account` WHERE `email`="'.$s.'"';
    $res = db_query($q);
    if (db_num_rows($res) <> 0) {
        return false;
    }
    return true;
}

function check_exist_char($s) {
    $q = 'SELECT `id` FROM `char` WHERE `name`="'.$s.'"';
    $res = db_query($q);
    if (db_num_rows($res) <> 0) {
        return false;
    }
    return true;
}


        ?>

        </div>
        </form>
    </div>
    </body>

</html>

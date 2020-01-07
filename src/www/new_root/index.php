<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Origin</title>

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

    <script src="js/jquery.js" type="text/javascript" charset="utf-8"></script>
    <link rel="stylesheet" href="css/prettyPhoto.css" type="text/css" media="screen" charset="utf-8" />
    <script src="js/jquery.prettyPhoto.js" type="text/javascript" charset="utf-8"></script>


    <style type="text/css">
        body { background: url("/images/bg.jpg") top center }   /*  50%   0% */


        #content
        {
            position:absolute;
            top:0;
            bottom:0;
            left:0;
            right:0;
        }


        .links_container
        {
            background-color: #f3f1dc;
            width: 650px;
            padding: 10px 10px 20px 15px;
            margin-top: 130px;
            margin-bottom: 30px;
            -webkit-border-radius: 10px;
            -moz-border-radius: 10 px;
            border-radius: 10px;
            border: 2px solid #4c2e1b;

            margin-left: auto;
            margin-right: auto;
            text-align: center;
        }
        .container2
        {
            background-color: #f3f1dc;
            width: 650px;
            padding: 10px 10px 20px 15px;
            margin: 30px;
            -webkit-border-radius: 10px;
            -moz-border-radius: 10 px;
            border-radius: 10px;
            border: 2px solid #4c2e1b;

            margin-left: auto;
            margin-right: auto;
            text-align: center;
        }
        .link
        {
            background-color: transparent;
            margin: 5px;
            display: inline;
            margin-left: 20px;
            margin-right: 20px;
            text-align: center;

        }
    </style>

</head>
<body>

    <script type="text/javascript" charset="utf-8">
      $(document).ready(function(){
        $("a[rel^='prettyPhoto']").prettyPhoto();
      });
    </script>


    <div id="content">
    <div class="links_container">
        <div class="link">
        <a href="signup.php">Registration</a>
        </div>
        <div class="link">
            <a href="download.html">Download</a>
        </div>
        <div class="link">
        <a href="/forum/">Forum</a>
        </div>
    </div>

        <div class="container2">

<?php
require_once 'online.php';

$res = get_online();
//$res = 0;

if ($res == 0) {
	echo 'Players online: 0 <br>Server status: OK';
} else {
	//if ((time() - $res['time']) > 30) $status = "Down"; else $status = "Ok";
	$status = 'OK';

    echo 'Players online: '.$res['count'].' <br>Server status: '.$status;

}

?>
        </div>
        <div class="container2">
            Screenshots<br>
<br>
<a href="images/shots/1.png" rel="prettyPhoto[pp_gal]"><img src="images/shots/1_sm.png" /></a>
<a href="images/shots/2.png" rel="prettyPhoto[pp_gal]"><img src="images/shots/2_sm.png" /></a><br>
<a href="images/shots/3.png" rel="prettyPhoto[pp_gal]"><img src="images/shots/3_sm.png" /></a>
<a href="images/shots/4.png" rel="prettyPhoto[pp_gal]"><img src="images/shots/4_sm.png" /></a><br>
            </div>
    </div>
</body>
</html>

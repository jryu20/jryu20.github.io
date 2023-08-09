#!/usr/local/bin/php
<?php
  if ($_FILES['fileUserSelected']['tmp_name'] != "") {
    $fileName = $_FILES['fileUserSelected']['name'];
    $currentLocation = $_FILES['fileUserSelected']['tmp_name'];
    $desiredLocation = __DIR__.'/'.$fileName;
    move_uploaded_file($currentLocation, $desiredLocation);

    header("Content-Disposition: attachment; filename=tidy_$fileName");

    $file = @fopen($fileName, 'r');
    if($file) {
      while(!feof($file)) {
        $line = fgets($file);
        if(substr($line, -2) == "\r\n") {
          $line = rtrim(substr($line, 0, strlen($line)-2), " ");
          echo $line;
          echo "\r\n";
        } else if(substr($line, -1) == "\n") {
          $line = rtrim(substr($line, 0, strlen($line)-1), " ");
          echo $line;
          echo "\n";
        } else {
          $line = rtrim($line, " ");
          echo $line;
        }
      } 

      fclose($file);
      unlink("$fileName");
    }

    exit;
  }       
?>

<!DOCTYPE html>
<html lang="en">

  <head>
    <meta charset="UTF-8">
    <title> PHP Demo </title>
  </head>

  <body>
    <header>
      <h1> PHP Demo - Tidy Trailing Space </h1>
    </header>

    <main>
      <form enctype="multipart/form-data" method="POST" action="<?php echo $_SERVER['PHP_SELF']; ?>">
        <input type="file" accept=".txt" name="fileUserSelected">
        <br>
        <input type="submit" name="submit">
      </form>

    </main>

    <footer>
      <hr>
      <small>
        &copy; Jun Ryu, 2022
      </small>
    </footer>

  </body>

</html>
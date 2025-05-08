const wordcount = 500;

var message = "";
var lastchar = '';

var linelength = 0;
for (i = 0; i < wordcount; i++) {
  var wordlength = 0;
  while ((Math.floor(Math.random() * 3) >= 1 || wordlength < 1) && wordlength < 30) {
    var newascii = Math.floor(Math.random() * 26);
    var newchar = String.fromCharCode(newascii+"a".charCodeAt(0));
    if (lastchar !== '' && Math.floor(Math.random() * 2) === 1) {
      newchar = lastchar;
    }
    message = message.concat(newchar);
    lastchar = newchar;
    wordlength++;
  }
  if (wordlength === 1) {
    if (Math.floor(Math.random() * 2) === 1) {
      newascii = Math.floor(Math.random() * 26);
      newchar = String.fromCharCode(newascii+"a".charCodeAt(0));
      if (lastchar !== '' && Math.floor(Math.random() * 2) === 1) {
        newchar = lastchar;
      }
      message = message.concat(newchar);
    }
  }
  if (i === wordcount-1) {
    message = message.concat('.');
  } else {
    if (linelength > 4 && Math.floor(Math.random() * 5) < 2) {
      message = message.concat('.\n');
      linelength = 0;
    } else {
      if (Math.floor(Math.random() * 10) < 1) {
        message = message.concat(',');
      }
      message = message.concat(' ');
      linelength++;
    }
  }
  
}

console.log(message);
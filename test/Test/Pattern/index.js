const Main = require("../../../output/Test.Pattern.Main");

const green = "#6f7";
const blue = "#67f";

function main () {
  console.log
    ( `%cStarting app. %cTime is ${new Date().toLocaleTimeString("nb-NO")}`
    , `color: ${green}`
    , `color: ${blue}`
    );

  Main.main();
}

main();
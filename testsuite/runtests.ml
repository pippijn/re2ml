(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "re2ml.native";
    suffixes = [".mll"];
    options = None;
    dirs = [
      "../src";
      "re2ml";
    ];
  };
])

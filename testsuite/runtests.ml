(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "re2ml.native";
    suffixes = [".mll"];
    dirs = [
      "../src";
      "re2ml";
    ];
  };
])

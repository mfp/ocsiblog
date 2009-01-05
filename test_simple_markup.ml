open OUnit
open Simple_markup
open Printf
open ExtString

let wrap f x =
  "\n" ^ f x ^ "\n"

let aeq_pars ?msg expected actual =
  assert_equal ?msg ~printer:(wrap string_of_paragraphs) expected actual

let check expected input =
  aeq_pars ~msg:(sprintf "With input:\n%s\n" (String.strip input))
    expected (parse_text input)

let test_read_list () =
  check
    [Ulist ([Normal [Text "foo "; Bold "bar"]], [[Normal [Text "baz"]]])]
    "* foo\n*bar*\n* baz";
  check
    [Ulist ([Normal [Text "foo bar baz"]], [[Normal [Text "baz"]]])]
    "* foo\nbar \n   baz\n* baz";
  check
    [Ulist ([Normal [Text "foo"]; Normal [Text "bar"]], [[Normal [Text "baz"]]])]
    "* foo\n\n bar\n* baz";
  check
    [Ulist ([Normal [Text "foo"]], [])]
    "* foo";
  check
    [Ulist ([Normal [Text "foo"]], [[Normal [Text "bar"]]])]
    "* foo\n* bar";
  check
    [Ulist ([Normal [Text "foo"]], [[Normal [Text "bar"]]])]
    "* foo\n\n* bar";
  check
    [Ulist ([Normal [Text "foo"]; Ulist ([Normal [Text "bar"]], [])],
            [])]
    "* foo\n\n * bar";
  check
    [Ulist ([Normal [Text "foo"]; Ulist ([Normal [Text "bar"]], []);
             Olist ([Normal [Text "1"]], [[Normal [Text "2"]]])],
            []);
     Olist ([Normal [Text "3"]], [])]
    "* foo\n\n * bar\n # 1\n # 2\n# 3";
  check
    [Ulist ([Normal [Text "foo"]; Ulist ([Normal [Text "bar"]], []);
             Olist ([Normal [Text "1"]], [[Normal [Text "2 #3"]]])],
            [])]
    "* foo\n\n * bar\n # 1\n # 2\n#3";
  check
    [Ulist
       ([Normal [Text "some paragraph"]; Normal [Text "And another one."]],
        [[Normal [Text "two"]]; [Normal [Text "three"]]])]
    "
     *   some
         paragraph

         And another one.

     *   two
     *   three
    ";
  check
    [Ulist ([Normal [Text "foo "; Bold "bar baz"]; Normal [Text "xxx"]],
            [[Normal [Text "baz"]]])]
    "*\tfoo\n*bar\n baz*\n\n xxx\n\n* baz";
  check
    [Normal [Text "foo"]; Ulist ([Normal [Text "bar"]], [])]
    "foo\n*\tbar";
  check
    [Olist ([Normal [Text "one"]],
            [[Normal [Text "two"]]; [Normal [Text "three"]]])]
    "
     #\tone
     #\ttwo
     #\tthree"

let test_read_normal () =
  check [Normal [Text "foo "; Struck [Text " bar baz "]; Text " foobar"]]
    "foo == bar\nbaz == foobar";
  check
    [Normal
       [Text "foo "; Bold "bar"; Text " "; Bold "baz"; Text " ";
        Emph "foobar"; Text " ";
        Link { href_target = "target"; href_desc = "desc"};
        Image { img_src = "image"; img_alt = "alt"};
        Text "."]]
    "foo *bar* *baz* _foobar_ [desc](target)![alt](image).";
  check
    [Normal [Bold "foo"; Text " "; Struck [Bold "foo"; Emph "bar"]]]
    "*foo* ==*foo*_bar_==";
  check
    [Normal
       [Link { href_target = "http://foo.com"; href_desc = "http://foo.com" }]]
    "[http://foo.com]()";
  check [Normal [Text ""]] "[]()";
  check
    [Normal
       [Text "foo "; Anchor "internal-link"; Text ". ";
        Link { href_target = "#internal-link"; href_desc = "back" }]]
    "foo [](#internal-link). [back](#internal-link)"

let test_read_normal_unmatched () =
  check [Normal [Text "foo * bar"]] "foo * bar";
  check [Normal [Text "foo _ bar"]] "foo _ bar";
  check [Normal [Text "foo == bar"]] "foo == bar";
  check [Normal [Text "foo == bar"]; Normal [Text "baz =="]] "foo == bar\n\nbaz =="

let test_read_pre () =
  check
    [Normal [Text "foo * bar"];
     Pre("a\n b\n  c\n", None);
     Pre("a\\0\\1\\2\n b\n  c\n", Some "whatever")]
    "foo * bar\n{{\na\n b\n  c\n}}\n\n{{whatever\na\\0\\1\\2\n b\n  c\n}}\n  ";
  check
    [Pre("a\n b\n  c\n", Some "foobar")]
    "{{foobar
     a
      b
       c
     }}";
  check
    [Pre("a\n b\n  c\n", Some "foo")]
    "  {{foo
         a
          b
           c
         }}";
  check
    [Pre("a\n }}\n  \\}}\n   }}}\n", None)]
    "{{
       a
        \\}}
         \\\\}}
          }}}
     }}"

let test_heading () =
  for i = 1 to 6 do
    check
      [Heading (i, [Text "foo "; Link { href_target = "dst"; href_desc = "foo" }])]
    (String.make i '!' ^ "foo [foo](dst)")
  done

let test_quote () =
  check [Quote [Normal [Text "xxx"]]] "> xxx";
  check [Quote [Normal [Text "xxx"]]] "> \n> xxx\n> ";
  check [Normal [Text "foo says:"];
         Quote [Normal [Text "xxx:"];
                Ulist ([Normal [Text "xxx yyy"]],
                       [[Normal [Emph "2"]]; [Normal [Bold "3"]]]);
                Quote [Normal [Text "yyy"]; Quote [Normal [Text "zzz"]];
                       Normal [Text "aaa"]]]]
    "foo says:\n\
     \n\
     > xxx:\n\
     > * xxx\n\
     >   yyy\n\
     > * _2_\n\
     > * *3*\n\
     > > yyy\n\
     > > > zzz\n\
     > > aaa\n\
     \n\
     ";
  check [Quote [Ulist ([Normal [Text "one"]; Normal [Text "xxx"]],
                       [[Normal [Text "two"]]])]]
    "> * one\n\
     >\n\
     >   xxx\n\
     > * two\n\
     \n"

let tests = "Simple_markup unit" >:::
  [
    "Normal" >:: test_read_normal;
    "Normal, unmatched delimiters" >:: test_read_normal_unmatched;
    "Ulist and Olist" >:: test_read_list;
    "Pre" >:: test_read_pre;
    "Heading" >:: test_heading;
    "Quote" >:: test_quote;
  ]

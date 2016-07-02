open Low

let add_n n = low
{
  var x : i32;
  var y : i32;
  { x = 0; }
  y = x + `num:n;
}

let _ =
  begin
    Export.as_function "add1" (add_n 1);
    Export.as_function "add2" (add_n 2);
  end

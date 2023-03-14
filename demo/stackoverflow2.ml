(* https://stackoverflow.com/questions/52323697/why-is-there-a-ocaml-syntax-error-on-if-statement *)

let rec list_above thresh lst =
  if lst = [] then
    printf("Herewego");
  else
    begin
  if (List.hd lst) >= thresh then
    (((List.hd lst)::(list_above thresh List.tl lst)))
  else if (List.hd lst) < thresh then
    ((list_above thresh List.tl lst));
end
;;

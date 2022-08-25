
open Printf;;

let graphps_version = "1.2";;
let picture_version = "0.5";;

exception Picture_failure of string;;

type obj =
   | CText of float * float * string
   | Segment of float * float * float * float

   | Rect  of float * float * float * float
   | RectF of float * float * float * float

   | Poly  of (float * float) array
   | PolyF of (float * float) array

   | Arc  of float * float * float * float * float * float
   | ArcF of float * float * float * float * float * float

   | Ellipse  of float * float * float * float
   | EllipseF of float * float * float * float

   | Circle  of float * float * float
   | CircleF of float * float * float

   | Set_line_width of int

   | Set_rgb_color of int * int * int
   | Plot of float * float

   | Moveto of float * float
   | Lineto of float * float

(* | Set_font of string * int *)
;;


type t = {
    objs : obj list;
    x:float;
    y:float;
    width : float;
    height : float
  } 


  
let bb_cmd cmd = 
  match cmd with
  | Set_rgb_color _
  | Set_line_width _ 
    -> None

  | CText(x,y,s) -> None

  | Segment(x1,y1,x2,y2) -> Some(min x1 x2, min y1 y2, max x1 x2, max y1 y2)

  | Rect (x, y, w, h) 
  | RectF (x, y, w, h) -> Some(x,y,x+.w,y+.h) (* min max ?*)

  | Plot (x, y) ->   Some(x,y,x,y)
  | Moveto (x, y) -> Some(x,y,x,y)
  | Lineto (x, y) -> Some(x,y,x,y)

  | Circle(x,y,r)
  | CircleF(x,y,r) -> let r = abs_float r in Some(x-.r,y-.r,x+.r,y+.r)

  | _ -> failwith "scale_cmd: unknown cmd";;



(* compute and set bounding box of a picture *)

let set_bb p = 
  match p with { objs=c; x=x; y=y; width=w; height=h }
    ->
      let fct  (xl,yb,xr,yt) cmd =
	match bb_cmd cmd with
	| None -> (xl,yb,xr,yt)
	| Some(x1,y1,x2,y2) -> ((min x1 xl), (min y1 yb), (max x2 xr), (max y2 yt))
      in
      let x1,y1,x2,y2 = List.fold_left fct 
	  (infinity,infinity,-.infinity,-.infinity) c
      in
      { objs=c; x=x1; y=y1; width=x2-.x1; height=y2-.y1 }
;; 

(* get current bounding box of a picture *)
let bb p = match p with
  { objs=_; x=x; y=y; width=w; height=h }
  -> x,y,w,h



(* functional style *)

let make_picture objs =
  set_bb
    {
     objs = List.rev objs;
     x=0.; y=0.; width=0.; height=0. 
   }
;;


open Graphics


(* -------------- screen export ----------------- *)

let screen_of_cmd dx dy sc cmd =
  let x_ x = (int_of_float (x *. sc)) + dx
  and y_ y = (int_of_float (y *. sc)) + dy
  and w_ w = (int_of_float (w *. sc))
  in
  match cmd with
  | Rect(xb,yb,w,h) -> draw_rect (x_ xb) (y_ yb) (w_ w) (w_ h)
  | RectF(xb,yb,w,h) -> fill_rect (x_ xb) (y_ yb) (w_ w) (w_ h)
  | Circle(xo,yo,r) -> draw_circle (x_ xo) (y_ yo) (w_ r)
  | CircleF(xo,yo,r) -> fill_circle (x_ xo) (y_ yo) (w_ r)
  | Poly(t) -> draw_poly_line (Array.map (fun (x,y) -> (x_ x, y_ y)) t) 

  | CText(x,y,s) -> let tx,ty = text_size s in
    moveto ((x_ x)-tx/2) ((y_ y)-ty/2); draw_string s

  | Segment(x1,y1,x2,y2) -> moveto (x_ x1) (y_ y1); lineto (x_ x2) (y_ y2)

  | Moveto(x,y) -> moveto (x_ x) (y_ y)
  | Lineto(x,y) -> lineto (x_ x) (y_ y)

  | _ -> ()

(* display pict on screen on window (xw,yw,ww,hw) *)

let to_screen pict xw yw ww hw =
  let x,y,w,h = bb pict in
  (* compute scale *)
  let sc = min ((float hw)/. h) ((float ww)/. w)
  in
  (* compute translation parameters *)
  let dx = xw - (int_of_float (x*.sc))
  and dy = yw - (int_of_float (y*.sc))
  in
  (* draw to screen *)
  List.iter (screen_of_cmd dx dy sc) (List.rev pict.objs)


(* display pict at scale at position (xw,yw) *)

let to_sc pict xw yw =
  let x,y,w,h = bb pict in
  to_screen pict xw yw (int_of_float w) (int_of_float h)
;;

(* ------------ postscript export ------------ *)


let eps_mode = ref true;;
let fonts : (string,string) Hashtbl.t = Hashtbl.create 11;;
let default_font = "Helvetica-Bold";;
let default_font_size = 10;;


(* Post Script functions embedded into the output *)
let ps_defs = "\
/ctext { dup stringwidth pop 2 div neg 0 rmoveto show } def
/cctext { dup stringwidth pop 2 div neg 
	  gsave newpath 0 0 moveto (!) false charpath flattenpath pathbbox
	  exch pop exch pop exch pop 2 div neg grestore
	  rmoveto show } def
/m { moveto } def
/rm { rmoveto } def
/l { lineto } def
/c { currentpoint } def
/cp { currentlinewidth currentpoint } def
/ms { c stroke m } def
/dl { l ms } def
/rl { rlineto ms } def
/p { newpath m c lineto 1 setlinewidth stroke m setlinewidth } def
/pr  %% define the path for a rectangle w h x y
    { newpath m
      dup %% w h h
      0 exch rlineto %% rlineto 0 h -- w h
      exch 0 rlineto %% rlineto w 0 -- h
      0 exch sub 0 exch rlineto %% rlineto 0 -h --
      closepath } def
/dr %% w h x y
    { pr stroke m } def
/fr { pr fill m } def
/fc { 0 360 newpath arc fill m } def
/dc { 0 360 newpath arc stroke m } def
/fa { gsave translate scale newpath 0 0 m arc closepath fill grestore } def
/da { savematrix gsave translate scale newpath arc
      restorematrix stroke grestore } def
/fe { gsave translate scale newpath 0 0 1 0 360 arc fill grestore } def
/de { savematrix gsave translate scale newpath 0 0 1 0 360 arc
      restorematrix stroke grestore } def
/pc { newpath m } def
/dp { closepath stroke m } def
/fp { closepath fill m } def
/ct { curveto c stroke m } def
/kcolor { 1 255 div } def
/color { kcolor mul } def
/stmp { /tmp exch def } def
/srgb {% r g b -
 color stmp color exch color exch tmp setrgbcolor } def
/t { show } def
/savedmatrix [0 0 0 0 0 0] def
/savematrix { savedmatrix currentmatrix pop } def
/restorematrix { savedmatrix setmatrix } def
%% ISO fonts
/ISOLatin1Encoding [
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclam /quotedbl /numbersign /dollar /percent /ampersand /quoteright
 /parenleft /parenright /asterisk /plus /comma /minus /period /slash
 /zero /one /two /three /four /five /six /seven
 /eight /nine /colon /semicolon /less /equal /greater /question
 /at /A /B /C /D /E /F /G /H /I /J /K /L /M /N /O
 /P /Q /R /S /T /U /V /W /X /Y /Z /bracketleft /backslash /bracketright
                                                       /asciicircum /underscore
 /quoteleft /a /b /c /d /e /f /g /h /i /j /k /l /m /n /o
 /p /q /r /s /t /u /v /w /x /y /z /braceleft /bar /braceright /asciitilde
                                                                       /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclamdown /cent /sterling /currency /yen /brokenbar /section
 /dieresis /copyright /ordfeminine /guillemotleft /logicalnot /hyphen
                                                            /registered /macron
 /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
                                                                /periodcentered
 /cedilla /onesuperior /ordmasculine /guillemotright /onequarter /onehalf
                                                   /threequarters /questiondown
 /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla
 /Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
                                                         /Idieresis
 /Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
 /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn /germandbls
 /agrave /aacute /acircumflex /atilde /adieresis /aring /ae /ccedilla
 /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
                                                         /idieresis
 /eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide
 /oslash /ugrave /uacute /ucircumflex /udieresis /yacute /thorn /ydieresis
] def
%% usage: isoname oldname makeisofont -
/makeisofont {
  dup findfont length dict dup begin
    exch findfont {
      exch dup /FID eq { pop pop } { exch def } ifelse
    } forall
    /Encoding ISOLatin1Encoding def
  end
  definefont pop
} def
";;

let make_iso_font f = sprintf
 "/ISO%s /%s makeisofont" f f;;

let hashtbl_assocs t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := (k, v) :: !res) t;
 !res;;

let hashtbl_vals t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := v :: !res) t;
 !res;;

let hashtbl_map f t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := f k v :: !res) t;
 !res;;

let make_iso_fonts () =
 let font_list = hashtbl_vals fonts in
 String.concat "\n" (List.map make_iso_font font_list);;

let set_default_font default_font default_font_size = sprintf
 "/ISO%s findfont %i scalefont setfont\n" default_font default_font_size;;

let creator x0 y0 width height = sprintf
  "%%!PS-Adobe-2.0 EPSF-2.0\n\
   %%%%Creator: GraphPs %s\n\
   %%%%BoundingBox: %f %f %f %f\n" graphps_version x0 y0 width height;;

let document_fonts () =
  let font_list = hashtbl_vals fonts in
  sprintf
   "%%%%DocumentFonts: %s\n\
    %%%%EndComments\n" (String.concat " " font_list);;

let header x0 y0 width height =
  sprintf "%s%s" (creator x0 y0 width height) (document_fonts ());;

let prelude x0 y0 width height default_font default_font_size =
  sprintf
   "%s\
    %%BeginProcSet\n\
    gsave\n\
    %s\
    0 0 m\n\
    1 setlinewidth\n\
    1 setlinecap\n\
    2 setlinejoin\n\
    10 setmiterlimit\n\
    %s\n\
    %s\
    %%EndProcSet\n" (header x0 y0 width height)
        ps_defs
        (make_iso_fonts ())
        (set_default_font default_font default_font_size);;

(* Could be "showpage", if one wants automatic display of the current page. *)
let postlude () =
 if !eps_mode then "grestore\n" else "grestore\nshowpage\n%%end\n";;

let escape_char b = function
  | '(' | ')' | '\\' as c -> Buffer.add_char b '\\'; Buffer.add_char b c
  | c -> Buffer.add_char b c;;

let escape_char_for_ps c =
 let b = Buffer.create 3 in
 escape_char b c;
 Buffer.contents b;;

let escape_string_for_ps s =
 let l = String.length s in
 let b = Buffer.create l in
 for i = 0 to l - 1 do escape_char b s.[i] done;
 Buffer.contents b;;

type filling = Fill | Draw;;

(*
let print_poly filling v =
  if Array.length v > 0 then begin
   let x, y = v.(0) in
   let s1 = sprintf "c %i %i pc\n" x y
   in
   s1 @
   for i = 1 to Array.length v - 1 do
     let x, y = v.(i) in
     fprintf oc "%i %i l\n" x y
   done;
   (if filling = Draw then "dp\n" else "fp\n")
  end;;
*)

let print_poly filling t =
  let rec h i acc =
    if i=Array.length t then acc 
    else h (i+1) (let x,y = t.(i) in (sprintf "%f %f l" x y) :: acc)
  in
  let x,y = t.(0) in
  String.concat "\n" ((sprintf "c %f %f pc" x y) ::
		      (List.rev (h 1 [])) @
		     (if filling = Draw then [ "stroke m\n" ] else [ "fp\n" ]))
;;


let ps_of_cmd = function
  | CText(x,y,s) -> sprintf "%f %f m (%s) cctext\n" x y s
  | Segment(x1,y1,x2,y2) -> sprintf "%f %f m %f %f dl\n" x1 y1 x2 y2

  | Set_rgb_color (r, g, b) ->
     sprintf "%i %i %i srgb\n" r g b
  | Plot (x, y) ->
     sprintf "cp %f %f p\n" x y
  | Moveto (x, y) ->
     sprintf "%f %f m\n" x y
  | Lineto (x, y) ->
     sprintf "%f %f dl\n" x y
  | Set_line_width w ->
     sprintf "%i setlinewidth\n" w

  | Rect (x, y, w, h) ->
     sprintf "c %f %f %f %f dr\n" w h x y
  | RectF (x, y, w, h) ->
     sprintf "c %f %f %f %f fr\n" w h x y
(*  | Fill_poly v ->
     print_poly oc Fill v
*)  | Poly v ->
     print_poly Draw v
  | Circle (x, y, r) ->
     sprintf "c %f %f %f dc\n" x y r
  | CircleF (x, y, r) ->
     sprintf "c %f %f %f fc\n" x y r
  | Ellipse (x, y, rx, ry) ->
     sprintf "%f %f %f %f de\n" rx ry x y
  | EllipseF (x, y, rx, ry) ->
     sprintf "%f %f %f %f fe\n" rx ry x y
  | Arc (x, y, rx, ry, a1, a2) ->
     sprintf "0 0 1 %f %f %f %f %f %f da\n" a1 a2 rx ry x y
  | ArcF (x, y, rx, ry, a1, a2) ->
     sprintf "0 0 1 %f %f %f %f %f %f fa\n" a1 a2 rx ry x y

;;

let to_eps oc pict =
  match pict with { objs=c; x=x; y=y; width=w; height=h } ->
    output_string oc (sprintf "%s" (prelude x y (x+.w) (y+.h) 
				      default_font default_font_size));
    List.iter (fun c -> output_string oc (ps_of_cmd c)) (List.rev c);
    output_string oc (sprintf "%s\n" (postlude ()))
  

let to_eps_file name pict =
  let oc = open_out name
  in
  to_eps oc pict; close_out oc


(* Drawing *)

let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y
  done
;;

let draw_poly_line =
  let draw points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      moveto savex savey;
    end in
  draw;;

let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;;


let transp = -1;;

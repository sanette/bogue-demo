(* Bogue demo for simple widgets and layouts *)

(* San Vu Ngoc 2019-2022 *)

open Bogue
module W = Widget
module L = Layout
module T = Trigger

open Tsdl

let section_title s =
  L.flat_of_w [W.label ~size:12 ~fg:Draw.(opaque grey) s]

let icon_button name =
  let fg = Draw.(opaque black) in
  W.button ~kind:Button.Switch
    ~label_on:(Label.icon ~fg name)
    ~label_off:(Label.icon ~fg:(Draw.(lighter (lighter fg))) name)
    ""

let hline width =
  let style = Style.(empty |> with_bg (color_bg Draw.(transp black))) in
  L.resident (W.box ~w:width ~h:1 ~style ())

let fable_title = "L'AVARE QUI A PERDU SON TRÉSOR"
let fable_ref = "Jean de la Fontaine, Livre IV, fable 20"
let fable = "L'usage seulement fait la possession.
Je demande à ces gens de qui la passion
Est d'entasser toujours, mettre somme sur somme,
Quel avantage ils ont que n'ait pas un autre homme.
Diogène là-bas est aussi riche qu'eux,
Et l'avare ici-haut comme lui vit en gueux.
L'homme au trésor caché qu'Esope nous propose,
            Servira d'exemple à la chose.
                Ce malheureux attendait,
Pour jouir de son bien, une seconde vie ;
Ne possédait pas l'or, mais l'or le possédait.
Il avait dans la terre une somme enfouie,
        Son coeur avec, n'ayant autre déduit
            Que d'y ruminer jour et nuit,
Et rendre sa chevance à lui-même sacrée.
Qu'il allât ou qu'il vînt, qu'il bût ou qu'il mangeât,
On l'eût pris de bien court, à moins qu'il ne songeât
A l'endroit où gisait cette somme enterrée.
Il y fit tant de tours qu'un Fossoyeur le vit,
Se douta du dépôt, l'enleva sans rien dire.
Notre avare, un beau jour ne trouva que le nid.
Voilà mon homme aux pleurs : il gémit, il soupire.
            Il se tourmente, il se déchire.
Un passant lui demande à quel sujet ses cris.
            C'est mon trésor que l'on m'a pris.
 Votre trésor ? où pris ? Tout joignant cette pierre.
             Eh sommes-nous en temps de guerre
Pour l'apporter si loin ? N'eussiez-vous pas mieux fait
De le laisser chez vous en votre cabinet,
            Que de le changer de demeure ?
Vous auriez pu sans peine y puiser à toute heure.
 A toute heure, bons Dieux ! ne tient-il qu'à cela ?
            L'argent vient-il comme il s'en va ?
Je n'y touchais jamais.  Dites-moi donc, de grâce,
Reprit l'autre, pourquoi vous vous affligez tant,
Puisque vous ne touchiez jamais à cet argent :
            Mettez une pierre à la place,
            Elle vous vaudra tout autant."

let demo () =
  let width = 400 in

  (* Page 1: check buttons, radio, and text scrolling *)

  let check_title = section_title "Check buttons" in
  let check1 = L.flat_of_w ~sep:2 [W.check_box (); W.label "check this"] in
  let check2 = L.flat_of_w ~sep:2 [W.check_box (); W.label "check that"] in
  let check3 = L.flat_of_w ~sep:2 [W.check_box (); W.label "or this"] in
  let check_layout = L.tower ~margins:0
      [check_title; check1; check2; check3] in

  let radio_title = section_title "Radio buttons. Only one can be selected." in
  let radio = Radiolist.vertical
      [|"select this";
        "or rather that";
        "maybe this";
        "worst case, this one"|]  in
  let radio_layout = L.tower ~margins:0 ~sep:0
      [radio_title;Radiolist.layout radio] in

  let icon_title = section_title "Icon buttons." in
  let icon1 = icon_button "play" in
  let icon2 = icon_button "pause" in
  let icon3 = icon_button "stop" in
  let icon4 = icon_button "repeat" in
  let icons = L.flat_of_w [icon1; icon2; icon3; icon4] in
  let icon_layout = L.tower ~margins:0 [icon_title; icons] in

  let text_title = section_title "Text formatting, and scrollbar." in
  let text_head = W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [bold (para fable_title)]) in
  let text = W.text_display ~w:width ~h:630 fable in
  let text_ref =  W.rich_text ~w:width ~h:20
      Text_display.(page [italic (para fable_ref)]) in
  let text_layout = L.tower [text_title; L.resident text_head;
                             L.resident text;
                             L.resident text_ref] in
  let text_container =  L.make_clip ~h:340 text_layout in

  let page1 = L.tower [icon_layout; hline width;
                       check_layout; hline width;
                       radio_layout; hline width;
                       text_container] in

  (* Page 2: text input ... *)

  let input_title = section_title "Text input" in
  let ti =  W.text_input ~prompt:"John Doo-Doo Wap" () in
  let input = L.flat_of_w ~align:Draw.Center
      [W.label "Enter your name:"; ti] in
  let input_layout = L.tower ~margins:0 [input_title; input] in

  let hello_title = section_title "Dynamic text" in
  let hello = W.label ~size:40 "Hello!" in
  let hello_action = W.map_text (fun s -> "Hello " ^ s ^ "!") in
  let c_hello = W.connect ti hello hello_action
      Sdl.Event.[text_input; key_down] in
  let hello_layout = L.tower ~margins:0
      [hello_title; L.resident ~w:width hello] in

  let slider_title = section_title "Progress bar or slider" in
  let slider = W.slider ~kind:Slider.HBar 100 in
  let percent = W.label "    0%" in
  let set_percent w x =
    Label.set (W.get_label w) (Printf.sprintf "%u%%" x) in
  let action w1 w2 _ =
    let x = Slider.value (W.get_slider w1) in
    set_percent w2 x in
  let events = List.flatten [T.buttons_down; T.buttons_up;
                             T.pointer_motion; [Sdl.Event.key_down]]  in
  let c_slider = W.connect slider percent action events in
  let slider_l = L.resident ~background:L.theme_bg slider in
  let slider_bar = L.flat ~align:Draw.Center [slider_l; L.resident percent] in
  let slider_layout = L.tower ~margins:0
      [slider_title; slider_bar] in

  let buttons_title = section_title "Push and toggle buttons" in
  let button_reset = W.button ~border_radius:10 "Reset" in
  let click _ =
    Slider.set (W.get_slider slider) 0;
    Label.set (W.get_label percent) " 0%"
    (* it would be great if this could be done automatically. *)
  in
  W.on_click ~click button_reset;
  let button_start = W.button  ~border_radius:10 ~kind:Button.Switch
      "Start computing" in
  let start_action b s ev =
    let bw = W.get_button b in
    let sw = W.get_slider s in
    let state = Button.state bw in
    if state then
      let rec loop () =
        let x = Slider.value sw in
        if x >= 100 || T.should_exit ev
        then (T.will_exit ev;
              Button.reset bw)
        else (Slider.set sw (x+1);
              set_percent percent (x+1);
              W.update s;
              T.nice_delay ev 0.1;
              loop ()) in
      loop ()
    else W.update percent in
  let c_button = W.connect ~priority:W.Replace
      button_start slider start_action T.buttons_up in

  let buttons_layout = L.tower ~margins:0
      [ buttons_title;
        L.flat_of_w [button_reset; button_start] ] in

  let image_title = section_title "Image display" in
  let image = W.image ~w:(width/2) "nenuphar.jpeg" in
  let image_layout = L.tower ~margins:0 [image_title; L.resident image] in

  let quit_title = section_title "Popup window" in
  let quit_btn = W.button ~border_radius:10 "QUIT" in
  let yes_action () = T.push_quit () in
  let no_action () = () in

  let quit_layout = L.tower ~margins:0 ~align:Draw.Center
      [quit_title; L.resident quit_btn] in

  let bottom = L.flat ~align:Draw.Max ~margins:0
      [image_layout; Space.hfill (); quit_layout] in
  L.set_width bottom width;

  let page2 = L.tower
      [input_layout; hline width;
       hello_layout; hline width;
       slider_layout; hline width;
       buttons_layout; hline width;
       bottom] in

  let tabs = Tabs.create ~slide:Avar.Right
      ["Page 1", page1; "More stuff", page2] in

  (* Notice we need to put this code after definition of the main layout. *)
  let release _ =
    Popup.yesno ~w:100 ~h:50 "Really quit?" ~yes_action ~no_action tabs
  in
  W.on_button_release ~release quit_btn;

  let board = Main.make [c_hello; c_slider; c_button] [tabs] in
  Main.run board;;

let () =
  demo ();;

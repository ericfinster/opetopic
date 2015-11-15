// 
//  opetopic-ix.js - UI Definitions for Opetopic
//

var ui_scheme = {
    rows:[
	{ view: "template", type: "header", template: "Opetopic!" },
	{ view: "menu", id: "main_menu",
	  data: [
	      { value: "File", submenu: [ "Load", "Save", "Export" ] },
	      { value: "Shape", submenu: [ "Extrude", "Drop" ] }
	  ]
	},
	{ template: "row2" },
	{ view: "resizer" },
	{ template: "Face display" , maxHeight: 250 }
    ]
};






function hideNavBar(){
    var toc = document.querySelector("#table-of-contents");
    toc.style.display = "none";
    // document.documentElement.style.setProperty('--main-width', '90%');
    document.querySelector("body")
        .style
        .setProperty("margin-left", "100px");
    
    // button.style.left = "10px";
}

function showNavBar(){
    var toc = document.querySelector("#table-of-contents");
    toc.style.display = "block";
    // document.documentElement.style.setProperty('--main-width', '100%');
    document.querySelector("body")
        .style
        .setProperty("margin-left", "250px");
    
    // button.style.left = "25%";
}

var buttonFlag = false; 

// button.addEventListener("click", function(){
//     if(buttonFlag == true) {
//         hideNavBar();
//         buttonFlag = false;
//     } else {
//         showNavBar();
//         buttonFlag = true;
//     }
// });


// var init = function(){
//     hideNavBar();
//     document.body.appendChild(button);
// }


var openTOCMenu = function(){
    var toc = document.querySelector("#table-of-contents");
    toc.style.setProperty("height", "100%");
    toc.style.setProperty("overflow-y", "scroll");
}

var closeTOCMenu = function(){
   
    var toc = document.querySelector("#table-of-contents");
    toc.scrollTop = 0 ;
    toc.style.setProperty("height", "50px");
    toc.style.setProperty("overflow-y", "hidden");
}

var initMobileTocMenu = function(){
    var toc = document.querySelector("#table-of-contents");
    var to  = document.querySelector("#table-of-contents h2");
    

    var btnTop = document.createElement("button")
    btnTop.textContent     = "Top";
    btnTop.style.top       = "20px";    
    btnTop.style.right     = "120px";   
    btnTop.style.position  = "fixed";
    // button.style.zIndex = 10000;

    btnTop.addEventListener("click", function(){
        document.querySelector("#content").scrollTop = 0;
    });   
    toc.appendChild(btnTop);


    var btnIndex = document.createElement("button")
    btnIndex.textContent     = "Index";
    btnIndex.style.top       = "20px";    
    btnIndex.style.right     = "30px";   
    btnIndex.style.position  = "fixed";
    // button.style.zIndex = 10000;
    btnIndex.addEventListener("click", function(){
        window.location = "index.html";
    });   
    toc.appendChild(btnIndex);

    
    var tocTitle = document.querySelector("#table-of-contents h2");
    tocTitle.textContent = "TOC";

    var tocOpened = false;
    
    to.addEventListener("click", function(){
        // console.log("tocOpened = " + tocOpened);
        if(!tocOpened){
            openTOCMenu();
            tocOpened = true;
        }
        else {            
            closeTOCMenu();
            tocOpened = false;
        }
    });

    
    var tocLinks = document.querySelectorAll("#table-of-contents a");
    tocLinks.forEach(a => a.addEventListener("click", closeTOCMenu));
}


var init = function(){
    if (window.screen.width <= 960)
        initMobileTocMenu();    
}


document.addEventListener("DOMContentLoaded", init, false);

// document.addEventListener("DOMContentLoaded", init, false);


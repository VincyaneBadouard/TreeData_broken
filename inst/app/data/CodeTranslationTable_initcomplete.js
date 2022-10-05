// This is to give the count n of rows in the grey line that can collapse the n rows
function(settings, json) {


  $("tr.dtrg-group").each(function(i) {

    var old = $(this).children( "td" ).html()
    var count = Object.keys($(this).nextUntil(".dtrg-group")).length -2;

    $(this).children( "td" ).html(old + " (" + count+ ") ");


  })


// This is to color cells depending on what oclumn they belong to so it is easy to keep track when scroll left to right



var colors = ["#A0ADC0",
"#A0B9C6",
"#A0C9CC",
"#A0D2C9",
"#A0D8C0",
"#A0DFB3",
"#A0E5A3",
"#B1EBA0",
"#C7F2A0",
"#E1F8A0",
"#FFFF9F",
"#DDFF9A",
"#B4FF95",
"#91FF9D",
"#8CFFCB",
"#87FFFF",
"#82FDFF",
"#7DC4FF",
"#7984FF",
"#AB74FF",
"#EF6FFF"];


var patterns = $("th.coloredcolumn").map(function() {
   // return  $(this).text();

      return  RegExp($(this).text());

})


       for (let i = 0; i < patterns.length / 2; i++) { // have to devide by two because a second "invisible" row of headers messes up the colors otherwise

        const pattern = patterns[i]

         $("th.coloredcolumn").each(function() {
             if (pattern.test($(this).html())) {
                 $(this).css({"background-color": colors[i]});
             }
           });

        $("input").each(function() {
          if (pattern.test($(this).attr('value'))) {
            $(this).parent("td").css({"background-color": colors[i]});
            }
     })

       }



}




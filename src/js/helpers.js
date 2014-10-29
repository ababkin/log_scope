$(document).on("click", "#tab-test a", function (e) {
  e.preventDefault()
  $(this).tab('show')
});

function scrollDown(){
  $("body").animate({ scrollTop: $("#bottom").offset().top }, 500);
  highlight();
}

function highlight(){
  $('.query').each(function(i, block) {
    hljs.highlightBlock(block);
  });
}

$(document).on("shown.bs.collapse", ".panel", function (e) {
  $("body").animate({ scrollTop: $(this).offset().top - 100 }, 500);
});

$(document).on("show.bs.collapse", ".panel", function (e) {
  $(".request").removeClass("current");
  $(this).find(".request").addClass("current");
});

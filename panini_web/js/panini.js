$(document).ready(function () {
  function myFunc() {
    var input = $("#grammarInput").val();
    $("#stage0-grammar").text(input);
  }
  myFunc();

  //either this
  $('#grammarInput').keyup(function () {
    $('#stage0-grammar').html($(this).val());
  });

  //or this
  $('#grammarInput').keyup(function () {
    myFunc();
  });

  //and this for good measure
  $('#grammarInput').change(function () {
    myFunc(); //or direct assignment $('#stage0-grammar').html($(this).val());
  });
});

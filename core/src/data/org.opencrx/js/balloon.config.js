/*
This file contains the default configuration options.
Default options can be edited in this file or changed after the Balloon object is
initiliazed as follows:

  var balloon = new Balloon;
  balloon.fontColor   = 'black';
  balloon.fontFamily  = 'Arial, sans-serif';
  balloon.fontSize    = '12pt';

*/

// Adds all the instance variables to the balloon object.
// Edit the values as required for your implementation.
BalloonConfig = function(balloon) {

  // ID of element to which balloon should be added
  // default = none (document.body is used)
  // This option may be required for mediawiki or other
  // implementations with complex stylesheets
  balloon.parentID = null;

  // properties of fonts contained in basic balloons (default black)
  balloon.fontColor   = 'black';
  balloon.fontFamily  = 'Arial, sans-serif';
  balloon.fontSize    = '9pt';
  balloon.whiteSpace = 'nowrap'; // or normal

  // minimum allowed balloon width (px)
  balloon.minWidth = 150;

  // maximum allowed balloon width (px)
  balloon.maxWidth = 600;

  // Delay before balloon is displayed (msec)
  balloon.delayTime = 500;

  // If fade-in/out is allowed
  balloon.allowFade = true;

  // time interval for fade-in (msec)
  balloon.fadeIn    = 300;

  // time interval for fade-out (msec)
  balloon.fadeOut   = 300;

  // Vertical Distance from cursor location (px)
  balloon.vOffset  = 10;

  // text-padding within the balloon (px)
  balloon.padding  = 10;

  // How long to display mousover balloons (msec)
  // false = 'always on'
  balloon.displayTime = 10000;

  // width of shadow (space aroung whole balloon; px)
  // Balloon can be zero if there is no shadow and the
  // edges of the balloon are also the edges of the image
  balloon.shadow   = 20;

  // images of balloon body.  If the browser is IE < 7, png alpha
  // channels will not work.  An optional alternative image can be
  // provided.  It should have the same dimensions as the default png image
  balloon.images        = '../../javascript/balloon/images';
  balloon.balloonImage  = 'balloon.png';    // with alpha channels
  balloon.ieImage       = 'balloon_ie.png'; // indexed color, transparent background

  // whether the balloon should have a stem
  balloon.stem          = true;

  // The height (px) of the stem and the extent to which the
  // stem image should overlaps the balloon image.
  balloon.stemHeight  = 32;
  balloon.stemOverlap = 3;

  // A stem for each of the four orientations
  balloon.upLeftStem    = 'up_left.png';
  balloon.downLeftStem  = 'down_left.png';
  balloon.upRightStem   = 'up_right.png';
  balloon.downRightStem = 'down_right.png';

  // A close button for sticky balloons
  // specify the width of your button image
  // if you do not use the default image provided
  balloon.closeButton   = 'close.png';
  balloon.closeButtonWidth = 16;



  /*
    This section allows support for AJAX, iframes and JavaScript in balloons
    If you have concerns about XSS vulnerabilities, set some or all of these
    values to false;
  */

  /// URL for default AJAX request handler
  balloon.helpUrl            = false;

  // Should AJAX be allowed at all?
  balloon.allowAJAX          = true;

  // Allow iframe elements in balloons?
  balloon.allowIframes       = true;

  // Allow javascript event handlers in balloons?
  balloon.allowEventHandlers = false;

  // Allow <script> elements in balloons?
  balloon.allowScripts       = false;

  // Escape all HTML characters -- this will be very
  // unnattractive unless your AJAX request returns plain
  // text.  short of disallowing AJAX entirely, This is the safe
  // way to go if you must have AJAX in an environment where
  // outside users can send text to the browser/balloon
  balloon.escapeHTML         = false;
}

// simple Box alternative
BoxConfig = function(box) {
  box.isBox = true;

  // ID of element to which box should be added
  // default = none (document.body is used)
  // This option may be required for mediawiki or other
  // implementations with complex stylesheets
  box.parentID = null;

  // properties of fonts contained in basic boxes (default black)
  box.fontColor   = 'black';
  box.fontFamily  = 'Arial, sans-serif';
  box.fontSize    = '9pt';

  // border and bgcolor for plain box
  box.bgColor     = 'whitesmoke';
  box.borderStyle = '1px solid black';

  // minimum allowed box width (px)
  box.minWidth = 150;

  // maximum allowed box width (px)
  box.maxWidth = 600;

  // Delay before box is displayed (msec)
  box.delayTime = 500;

  // If fade-in/out is allowed
  box.allowFade = false;

  // time interval for fade-in (msec)
  box.fadeIn    = 300;

  // time interval for fade-out (msec)
  box.fadeOut   = 300;

  // Vertical Distance from cursor location (px)
  box.vOffset  = 5;

  // text-padding within the box (px)
  box.padding  = 10;

  // How long to display mousover boxes (msec)
  // false = 'always on'
  box.displayTime = 10000;

  // no shadows for plain box
  box.shadow   = 0;

  // no stem for boxes
  box.stem        = false;

  // A close button for sticky boxes
  // specify the width of your button image
  // if you do not use the default image provided
  box.images        =  '../../images/balloons';
  box.closeButton   = 'close.png';
  box.closeButtonWidth = 16;

  /*
    This section allows support for AJAX, iframes and JavaScript in boxes
    If you have concerns about XSS vulnerabilities, set some or all of these
    values to false;
  */

  /// URL for default AJAX request handler
  box.helpUrl            = false;

  // Should AJAX be allowed at all?
  box.allowAJAX          = true;

  // Allow iframe elements in boxes?
  box.allowIframes       = true;

  // Allow javascript event handlers in boxes?
  box.allowEventHandlers = false;

  // Allow <script> elements in boxes?
  box.allowScripts       = false;

  // Escape all HTML characters -- this will be very
  // unnattractive unless your AJAX request returns plain
  // text.  short of disallowing AJAX entirely, This is the safe
  // way to go if you must have AJAX in an environment where
  // outside users can send text to the browser/box
  box.escapeHTML         = false;
}

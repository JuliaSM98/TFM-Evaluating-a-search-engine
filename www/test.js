function hola() {
    const state = {};

    (function (setup) {
        if (typeof window.addEventListener !== 'undefined') {
            window.addEventListener('load', setup, false);
        } else if (typeof document.addEventListener !== 'undefined') {
            document.addEventListener('load', setup, false);
        } else if (typeof window.attachEvent !== 'undefined') {
            window.attachEvent('onload', setup);
        } else {
            if (typeof window.onload === 'function') {
                const oldonload = onload;
                window.onload = function () {
                    oldonload();
                    setup();
                };
            } else {
                window.onload = setup;
            }
        }
    })(function () {
        state.isOverIFrame = false;
        state.firstBlur = false;
        state.hasFocusAcquired = false;

        findIFramesAndBindListeners();

        document.body.addEventListener('click', onClick);

        if (typeof window.attachEvent !== 'undefined') {
            console.log('11');
            top.attachEvent('onblur', function () {
                state.firstBlur = true;
                state.hasFocusAcquired = false;
                onIFrameClick()
            });
            top.attachEvent('onfocus', function () {
                state.hasFocusAcquired = true;
                console.log('attachEvent.focus');
            });
        } else if (typeof window.addEventListener !== 'undefined') {
            console.log('22');
            top.addEventListener('blur', function () {
                state.firstBlur = true;
                state.hasFocusAcquired = false;
                onIFrameClick();
                console.log(HTMLElement);
            });
            top.addEventListener('focus', function () {
                state.hasFocusAcquired = true;
                console.log('addEventListener.focus');
            },false);
        }

        setInterval(findIFramesAndBindListeners, 500);
    });

    function isFF() {
        return navigator.userAgent.search(/chrome/i) !== -1;
    }

    function isActiveElementChanged() {
        const prevActiveTag = document.activeElement.tagName.toUpperCase();
        document.activeElement.blur();
        const currActiveTag = document.activeElement.tagName.toUpperCase();
        return !prevActiveTag.includes('BODY') && currActiveTag.includes('BODY');
    }

    function onMouseOut() {
        if (!state.firstBlur && isFF() && isActiveElementChanged()) {
            console.log('firefox first click');
            onClick();
        } else {
            document.activeElement.blur();
            top.focus();
        }
        state.isOverIFrame = false;
        console.log('onMouseOut');
    }

    function onMouseOver() {
    	document.activeElement.blur();
        top.focus();
        state.isOverIFrame = true;
        console.log('onMouseOver');
    }

    function onIFrameClick() {
        console.log('onIFrameClick');
        if (state.isOverIFrame && !state.hasFocusAcquired && state.firstBlur ) {
            onClick();
            state.firstBlur=false;
            //state.hasFocusAcquired = true;
        }
    }
    setInterval(onIFrameClick);

    function onClick() {
        console.log('onClick');
    }

    function findIFramesAndBindListeners() {
        return Array.from(document.getElementsByTagName('iframe')).forEach(function (element) {
                element.onmouseover = onMouseOver;
                element.onmouseout = onMouseOut;
            });
    }
}
hola();

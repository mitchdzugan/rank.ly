const isServer = () => (
    !(typeof window != 'undefined' && window.document)
);

const getTopId = () => {
    if (isServer()) {
        return "";
    }

    const ids = [
        'education',
        'work-exp',
        'smashgg',
        'everymove',
        'msft',
        'projects',
        'impulse',
        'liverpool',
        'ssbm',
        'santa',
        'mpp',
        'etc'
    ];
    let topId;
    ids.forEach((id) => {
        if (topId) {
            return;
        }

        const el = document.getElementById(id);
        if (!el) {
            return;
        }

        if (el.getBoundingClientRect().y > -10) {
            topId = id;
        }
    });

    return topId || "";
};

exports.getTopId = getTopId;

exports.scrollToId = (id) => () => {
    const el = document.getElementById(id);

    if (!el) {
        return;
    }

    el.scrollIntoView({ behavior: 'smooth' });
};

exports.attachScrollWatch = (push) => () => {
    if (isServer()) {
        return () => {};
    }

    window.onscroll = () => {
        push(getTopId())();
    };
    return () => {
        window.onscroll = () => {};
    };
};

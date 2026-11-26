// Research page: seating chart with a gazing speaker, theme filter and abstracts.
(function () {
  /* ---------- abstracts (Research page and landing page) ---------- */
  document.querySelectorAll('.rx-abstract-toggle').forEach(function (button) {
    button.addEventListener('click', function () {
      var panel = document.getElementById(button.getAttribute('aria-controls'));
      var open = button.getAttribute('aria-expanded') === 'true';
      button.setAttribute('aria-expanded', String(!open));
      panel.hidden = open;
    });
  });

  // Everything below belongs to the Research page, which has the seating chart.
  var root = document.querySelector('.rx');
  if (!root || !root.querySelector('.rx-chamber')) return;

  var items = Array.prototype.slice.call(root.querySelectorAll('.rx-item'));
  var filters = Array.prototype.slice.call(root.querySelectorAll('.rx-filter'));
  var order = filters.map(function (b) { return b.dataset.themeKey; });
  var reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var active = null;
  var seats = [];

  /* ---------- seating chart: one seat per paper, themes sit as blocs ---------- */
  var chamber = root.querySelector('.rx-chamber');
  var svg = chamber.querySelector('svg');
  var caption = chamber.querySelector('.rx-caption');
  var defaultCaption = caption.textContent;
  var NS = 'http://www.w3.org/2000/svg';

  function el(name, attrs, parent) {
    var node = document.createElementNS(NS, name);
    for (var k in attrs) node.setAttribute(k, attrs[k]);
    parent.appendChild(node);
    return node;
  }

  function seatPositions(n) {
    // Two concentric rows; the outer row holds proportionally more seats.
    var radii = [132, 200];
    var inner = Math.round(n * radii[0] / (radii[0] + radii[1]));
    var counts = [inner, n - inner];
    var pos = [];
    counts.forEach(function (count, row) {
      for (var i = 0; i < count; i++) {
        var t = count === 1 ? 0.5 : i / (count - 1);
        var angle = Math.PI * (1 - t); // left to right
        pos.push({ angle: angle, x: 220 + radii[row] * Math.cos(angle), y: 225 - radii[row] * Math.sin(angle) });
      }
    });
    // Fill from left to right so that each theme forms a wedge.
    return pos.sort(function (a, b) { return b.angle - a.angle; });
  }

  var gazeCone = el('path', { 'class': 'rx-cone', 'aria-hidden': 'true' }, svg); // first, so it sits behind the seats

  var seated = items.slice().sort(function (a, b) {
    return (order.indexOf(a.dataset.themeKey) - order.indexOf(b.dataset.themeKey)) ||
           (b.dataset.published === 'true') - (a.dataset.published === 'true');
  });

  var positions = seatPositions(seated.length);
  var jump;
  positions.forEach(function (p, i) {
    var item = seated[i];
    var g = el('g', { 'class': 'rx-seat', tabindex: '0', role: 'button', 'aria-label': item.dataset.title }, svg);
    g.dataset.themeKey = item.dataset.themeKey;
    g.dataset.published = item.dataset.published;
    el('circle', { 'class': 'rx-hit', cx: p.x, cy: p.y, r: 23, fill: 'transparent' }, g);
    el('circle', { 'class': 'rx-mark', cx: p.x, cy: p.y, r: 16 }, g);

    function show() { caption.textContent = item.dataset.title; }
    function hide() { caption.textContent = defaultCaption; }
    function go() {
      apply(item.dataset.themeKey); // stays selected until its filter button is clicked again
      // Give the speaker a moment to look over before the page moves on.
      clearTimeout(jump);
      jump = setTimeout(function () {
        item.scrollIntoView({ behavior: reduceMotion ? 'auto' : 'smooth', block: 'center' });
        item.classList.remove('rx-flash');
        void item.offsetWidth; // restart the highlight
        item.classList.add('rx-flash');
      }, reduceMotion ? 0 : 900);
    }
    g.addEventListener('mouseenter', show);
    g.addEventListener('mouseleave', hide);
    g.addEventListener('focus', show);
    g.addEventListener('blur', hide);
    g.addEventListener('click', go);
    g.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); go(); }
    });
    seats.push(g);
    positions[i].key = item.dataset.themeKey;
  });

  /* ---------- the speaker, who looks at the selected faction ---------- */
  var EYE = { x: 220, y: 181 };
  var speaker = el('g', { 'class': 'rx-speaker', 'aria-hidden': 'true' }, svg);
  el('path', { 'class': 'rx-shoulders', d: 'M184 232 C186 210 200 204 220 204 C240 204 254 210 256 232 Z' }, speaker);
  var head = el('g', { 'class': 'rx-head' }, speaker);
  el('circle', { 'class': 'rx-face', cx: 220, cy: 183, r: 26 }, head);
  var pupils = [-10.5, 10.5].map(function (dx) {
    el('circle', { 'class': 'rx-eye', cx: EYE.x + dx, cy: EYE.y, r: 8 }, head);
    return el('circle', { 'class': 'rx-pupil', cx: EYE.x + dx, cy: EYE.y, r: 3.6 }, head);
  });
  el('path', { 'class': 'rx-podium', d: 'M182 223 H258 L249 250 H191 Z' }, speaker);
  el('rect', { 'class': 'rx-lip', x: 175, y: 218, width: 90, height: 7, rx: 2 }, speaker);
  speaker.classList.add('rx-following');
  chamber.hidden = false;

  function point(angle, radius) { // angle in radians, counter-clockwise from "right"
    return (EYE.x + radius * Math.cos(angle)).toFixed(1) + ' ' + (EYE.y - radius * Math.sin(angle)).toFixed(1);
  }

  var cursor = null;  // last mouse position in the window
  var locked = false; // true while the speaker is looking at a faction

  function turn(ux, uy, headShift, pupilShift) {
    head.style.transform = 'translate(' + (ux * headShift) + 'px, ' + (uy * headShift * 0.6) + 'px)';
    pupils.forEach(function (n) { n.style.transform = 'translate(' + (ux * pupilShift) + 'px, ' + (uy * pupilShift) + 'px)'; });
  }

  // Without a selected faction, the eyes follow the cursor a little.
  function follow() {
    if (locked) return;
    if (!cursor || reduceMotion) { turn(0, 0, 0, 0); return; }
    var box = svg.getBoundingClientRect(), scale = box.width / 440;
    var dx = cursor.x - (box.left + EYE.x * scale), dy = cursor.y - (box.top + EYE.y * scale);
    var dist = Math.hypot(dx, dy) || 1;
    var strength = Math.min(1, dist / (120 * scale));
    turn(dx / dist * strength, dy / dist * strength, 3, 3.5);
  }

  var frame = null;
  function schedule() {
    if (!frame) frame = requestAnimationFrame(function () { frame = null; follow(); });
  }
  window.addEventListener('pointermove', function (e) {
    if (e.pointerType === 'touch') return;
    cursor = { x: e.clientX, y: e.clientY };
    schedule();
  }, { passive: true });
  window.addEventListener('scroll', schedule, { passive: true });
  document.documentElement.addEventListener('mouseleave', function () { cursor = null; schedule(); });

  function gaze(key) {
    var bloc = positions.filter(function (p) { return p.key === key; });
    locked = bloc.length > 0;
    speaker.classList.toggle('rx-following', !locked);
    if (!locked) {
      gazeCone.style.opacity = 0;
      follow();
      return;
    }
    var angles = bloc.map(function (p) {
      var a = Math.atan2(EYE.y - p.y, p.x - EYE.x);
      return a < -Math.PI / 2 ? a + 2 * Math.PI : a; // keep the left-hand side continuous
    });
    var from = Math.min.apply(null, angles), to = Math.max.apply(null, angles);
    var mid = (from + to) / 2, ux = Math.cos(mid), uy = -Math.sin(mid);
    var reach = Math.max.apply(null, bloc.map(function (p) {
      return Math.hypot(p.x - EYE.x, p.y - EYE.y);
    })) + 22;
    turn(ux, uy, 5, 4);
    gazeCone.setAttribute('d', 'M' + EYE.x + ' ' + EYE.y +
      ' L' + point(from - 0.09, reach) +
      ' A' + reach + ' ' + reach + ' 0 ' + (to - from > Math.PI ? 1 : 0) + ' 0 ' + point(to + 0.09, reach) + ' Z');
    gazeCone.setAttribute('data-theme-key', key);
    gazeCone.style.opacity = 1;
  }

  /* ---------- theme filter ---------- */
  function apply(key) {
    active = key;
    filters.forEach(function (b) { b.setAttribute('aria-pressed', String(b.dataset.themeKey === key)); });
    items.forEach(function (item) { item.hidden = !!key && item.dataset.themeKey !== key; });
    seats.forEach(function (s) { s.classList.toggle('rx-dim', !!key && s.dataset.themeKey !== key); });
    gaze(key);
    root.querySelectorAll('.rx-section').forEach(function (section) {
      section.classList.toggle('rx-empty', !section.querySelector('.rx-item:not([hidden])'));
    });
  }
  filters.forEach(function (b) {
    b.addEventListener('click', function () {
      clearTimeout(jump);
      apply(active === b.dataset.themeKey ? null : b.dataset.themeKey);
    });
  });

})();

# Browser benchmark 2026-11-10T16:09:25.810Z

Machine: 512 CPUs, AMD EPYC 9755 128-Core Processor, Chromium 156.0.8078.79, Node v22.22.3.

Method: keyed and filter times run in the page: performance.now() before element.click() (or the input event), end when a MessageChannel message posted from the next requestAnimationFrame callback arrives, that is after the frame's style, layout and paint. Chromium runs with --disable-gpu-vsync --disable-frame-rate-limit, so the next frame starts at once and the time does not include waiting for vsync. Each measured run uses a fresh browser context and page, with 1 untimed setup+operation rounds first in the same page. CPU throttling uses CDP Emulation.setCPUThrottlingRate after the app is loaded. Memory: CDP HeapProfiler.collectGarbage then Runtime.getHeapUsage usedSize. Page loads: fresh context (cold HTTP cache), throttle set on a same-origin page first, first-contentful-paint entry and the time a MutationObserver sees data-silica-ready on the root; the CPU check loop (5M iterations, first load) shows the throttle is active. Live: two pages of mini Linear in separate contexts; Date.now() in the writer page just before the Create issue button is clicked, and Date.now() in the watcher page when a MutationObserver sees the new title in the list (one machine, one clock): the POST, the handler, Server.publish, the WebSocket message, the step and the paint.

Times in ms: median (25th–75th percentile).

## Live: create in one page to the DOM change in another (mini Linear)

| issues | CPU | create press to DOM change |
|---|---|---|
| 30 | 1x | 4.0 (4.0–4.0) |
| 1000 | 1x | 17.0 (17.0–17.0) |

{application, videoreader,
[{description, "videoreader"},
 {vsn, "0.1"},
 {modules, [
    video_reader,
    videoreader,
    videoreader_app,
    videoreader_event,
    videoreader_sup
  ]},
 {registered,[videoreader]},
 {applications, [kernel,stdlib]},
 {mod, {videoreader_app,[]}}
]}.


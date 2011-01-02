-module(smtp).

-include("config.hrl").

-export([sendMail/3]).

sendMail(To, Message, Subject) ->
    log:write(trace, ?MODULE_STRING, "Sending mail to: ~s~n", [To]),
    Config = utils:readConfig(?CONFIG_PATH),
    SmtpFrom = server:get(Config, smtpFrom),
    Port = open_port({spawn, "sendmail " ++ To}, [{line, 1000}]),
    port_command(Port, lists:flatten("From: " ++ SmtpFrom ++ [10])),
    port_command(Port, lists:flatten("To: " ++ To ++ [10])),
    port_command(Port, lists:flatten("Subject: " ++ Subject ++ [10] ++ [10])),
    port_command(Port, lists:flatten(Message ++ [10])),
    port_command(Port, lists:flatten("." ++ [10])),
    log:write(trace, ?MODULE_STRING, "Mail sent~n", []).
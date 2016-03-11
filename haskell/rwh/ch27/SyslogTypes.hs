
module SyslogTypes where
{- | Priorities define how important a log message is. -}

data Priority = 
            DEBUG                   -- ^ Debug messages
          | INFO                    -- ^ Information
          | NOTICE                  -- ^ Normal runtime conditions
          | WARNING                 -- ^ General Warnings
          | ERROR                   -- ^ General Errors
          | CRITICAL                -- ^ Severe situations
          | ALERT                   -- ^ Take immediate action
          | EMERGENCY               -- ^ System is unusable
                    deriving (Eq, Ord, Show, Read, Enum)

{- | Facilities are used by the system to determine where messages
are sent. -}

data Facility = 
              KERN                      -- ^ Kernel messages
              | USER                    -- ^ General userland messages
              | MAIL                    -- ^ E-Mail system
              | DAEMON                  -- ^ Daemon (server process) messages
              | AUTH                    -- ^ Authentication or security messages
              | SYSLOG                  -- ^ Internal syslog messages
              | LPR                     -- ^ Printer messages
              | NEWS                    -- ^ Usenet news
              | UUCP                    -- ^ UUCP messages
              | CRON                    -- ^ Cron messages
              | AUTHPRIV                -- ^ Private authentication messages
              | FTP                     -- ^ FTP messages
              | LOCAL0                  
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

facToCode = [ 
                       (KERN, 0),
                       (USER, 1),
                       (MAIL, 2),
                       (DAEMON, 3),
                       (AUTH, 4),
                       (SYSLOG, 5),
                       (LPR, 6),
                       (NEWS, 7),
                       (UUCP, 8),
                       (CRON, 9),
                       (AUTHPRIV, 10),
                       (FTP, 11),
                       (LOCAL0, 16),
                       (LOCAL1, 17),
                       (LOCAL2, 18),
                       (LOCAL3, 19),
                       (LOCAL4, 20),
                       (LOCAL5, 21),
                       (LOCAL6, 22),
                       (LOCAL7, 23)

           ]


codeToFac = map (\(x, y) -> (y, x)) facToCode


{- | We can't use enum here because the numbering is discontiguous -}
codeOfFac :: Facility -> Int
codeOfFac f = case lookup f facToCode of
                Just x -> x
                _ -> error $ "Internal error in codeOfFac"

facOfCode :: Int -> Facility
facOfCode f = case lookup f codeToFac of
                Just x -> x
                _ -> error $ "Invalid code in facOfCode"

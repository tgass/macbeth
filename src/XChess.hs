module Main where

import Control.Concurrent.Chan
import FicsConnection2 (ficsConnection)
import WxLogin

import Graphics.UI.WX

main :: IO ()
main = do
  chan <- newChan
  h <- ficsConnection $ \h cmd -> writeChan chan cmd
  start $ wxLogin h chan






--iv_lock
--
--  Setting ivariable lock stops the further changing of ivariables until the
--user logs out and in again. This is to stop users tampering with the ivariables
--and causing the interface to malfunction.
--
--See Also:  iset ivariables
--
--[Last modified: August 1st, 2000 -- DAV]
--
--



--iv_seekinfo
--
--  Setting ivariable seekinfo provides the interface with extra notifications
--of sought offers. This allows an interface to easily track the seek ads
--so graphs can be presented to the user without having to resort to parsing
--the seeking messages.
--
--  To enable this mode:
--
--iset seekremove 1
--iset seekinfo 1
--set seek 1
--
--  At this point there will be an update of the current status of seek ads
--which looks like:
--
--- <sc>
--- <s> 8 w=visar ti=02 rt=2194  t=4 i=0 r=r tp=suicide c=? rr=0-9999 a=t f=f
--- <s> 12 w=saeph ti=00 rt=1407  t=1 i=0 r=r tp=lightning c=? rr=0-9999 a=t f=f
--
--  (note the - was added so as not to confuse interfaces displaying this
--   helpfile)
--
--- <sc> indicates that the interface should clear its current record of seek
--- requests.
--
--  The <s> message looks like:
--
--- <s> index w=name_from ti=titles rt=rating t=time i=increment
--      r=rated('r')/unrated('u') tp=type c=color
--      rr=rating_range(lower-upper) a=automatic?('t'/'f')
--      f=formula_checked('t'/f')
--
--Note titles is two hex digits without the 0x 'ored' together:
--  0x1 - unregistered
--  0x2 - computer
--  0x4 - GM
--  0x8 - IM
--  0x10 - FM
--  0x20 - WGM
--  0x40 - WIM
--  0x80 - WFM
--
--(so 0x3 would indicate an unregistered user on the computer list). It is
--unlikely the titles will ever be mixed though as they should be exclusive.
--However the server does allow this.
--
--Note rating is up to 4 digits with a provshow character following:
--  ' ' - established
--  'E' - estimated
--  'P' - provisional (never was established)
--
--Note color is one of the following:
--  '?' - don't care
--  'W' - wants white
--  'B' - wants black
--
--  When new seeks come in they will be shown in the <s> format. If the user
--changes his formula then all seeks will be shown again. Currently there is
--no update if a seeker with formula checking changes formula, or a rating
--change occurs.
--
--  Note: seeks are only shown when seek is 1 and there is no game in progress.
--To get another update reissue iset seekinfo 1.
--
--Finally the for removed seeks (seekremove must be set) the message:
--
--- <sr> 8 10   - Indicates indexes 8 and 10 have been removed
--                (seekremove must be set).
--
--  Note there are prompts following this seekinfo which should be parsed out to
--avoid the user seeing extra prompts as well as the seekinfo set message when
--an update occurs.
--
--  Note any new fields will be appended to the end so the interface must be
--able to handle this.
--
--  (Note on current FICS this is not correctly supported since setting lock
--   prevents an update.)
--
--See Also:  iset ivariables iv_lock
--
--[Last modified: August 3rd, 2000 -- DAV]


--  0x80 - WFM
--
--
--
-- TextMessage {message = "<sc>"}
-- TextMessage {message = "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t"}
-- TextMessage {message = "<sr> 59"}


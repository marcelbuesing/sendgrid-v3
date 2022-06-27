# Revision history for sendgrid-v3

## 1.0.0.0 -- Unreleased

- Fumiaki Kinoshita: make the _mailAddressName field Text again; toJSON omits it if it's empty (#25) (dd9b795)
- Aditya Manthramurthy: Attempt to fix CI (#23) (43cb429)
- Aditya Manthramurthy: Fix tests (#24) (b1e2ce3)
- Fumiaki Kinoshita: [major bump] provide headers as an object, not array (#22) (6c5bcf0)
- Aditya Manthramurthy: Enable Github CI (840dbf7)
- Aditya Manthramurthy: Build with GHC 8.10.3 (7a9394f)

## 0.3.0.0 -- 2020-08-25
- marcelbuesing: Add deployment version condition. (22f55f9)
- Aditya Manthramurthy: Allow building with GHC 8.10 and add build matrix for travis (b9ec01e)
- Aditya Manthramurthy: Allow compilation on GHC 8.8 (aaacc77)
- Tristan McLeay: fix docs: ensure env vars are colllated (4a14207)
- marcelbuesing: Setup hackage deployment on tagged builds (e4de4f6)
- 5outh: Make MailContent nullable (f33c9eb)
- 5outh: Serialize Substitutions as an Object, not an Array (82f02ac)
- 5outh: Omit Nothing fields in Personalization (7dd1492)
- 5outh: Update cabal version (2645d68)
- 5outh: Fix JSON serialization of SendGrid payloads (f174c9e)
- 5outh: Update README/Doc scripts (10666aa)
- 5outh: Add version bounds to additional dependencies (881cd35)

## 0.2.2.0 -- 2019-05-10
- 5outh: Bump MAJOR version due to backwards-incompatible changes (72fae76)
- 5outh: Make sendMail return more information (abf2931)

## 0.1.2.0 -- 2019-01-20
- Finn Espen Gundersen: Explicit JSON encode to allow easing version constraints of base and lens.
- Patrick Brisbin: Support for dynamic_template_data
  https://sendgrid.com/docs/ui/sending-email/how-to-send-an-email-with-dynamic-transactional-templates/
- Steven Leiva: Fixed example code in README and source.

## 0.1.1.0  -- 2018-08-16
- Relax base bounds to allow any 4.11 version

## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

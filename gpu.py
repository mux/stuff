#!/usr/bin/env python
# vi:ts=4:sw=4:et

# Mass phone numbers updater for Google contacts.
#
# Example usage:
#
# ./gpu.py -u <user> -p <password> -s '-' '' -s ' ' '' -s '^0(\d{9})$' '+33\1'
#
# This would strip spaces and '-' characters, and format 10-digit phone
# numbers starting with a 0 (supposedly french ones) to use the international
# +33 ... notation (useful when you're traveling).

from __future__ import print_function
from __future__ import unicode_literals

__author__ = 'Maxime Henrion <mhenrion@gmail.com>'

import argparse
import re

# GData related imports
import gdata.data
import gdata.contacts.data
import gdata.contacts.client

# Known phone number types at the time of this writing (for display only).
phone_types = {
    gdata.data.FAX_REL:      'fax',
    gdata.data.HOME_REL:     'home',
    gdata.data.HOME_FAX_REL: 'home fax',
    gdata.data.MOBILE_REL:   'mobile',
    gdata.data.OTHER_REL:    'other',
    gdata.data.PAGER_REL:    'pager',
    gdata.data.WORK_REL:     'work',
    gdata.data.WORK_FAX_REL: 'work fax'
}

# Return all contacts using python generators for convenience.
def get_all_contacts(client, size=128):
    query = gdata.contacts.client.ContactsQuery()
    query.start_index = 1
    query.max_results = size

    done = False
    while not done:
        feed = client.GetContacts(q=query)
        num = 0
        for contact in feed.entry:
            yield contact
            num += 1
        if num < size:
            done = True
        else:
            query.start_index += size

def update_contact(contact, subs):
    updates = []
    for phone in contact.phone_number:
        old = phone.text
        new = old
        for pattern, repl in subs:
            new = re.sub(pattern, repl, new)
        if new != old:
            phone.text = new
            updates.append((phone, old))
    return updates

def confirm(prompt, choices=None, default=None):
    if choices is None:
        choices = ('yY', 'nN')
    ans = raw_input(prompt)
    for i, choice in enumerate(choices):
        if ans in choice:
            return choice[0]
    return default

parser = argparse.ArgumentParser()
parser.add_argument('-u', '--user', required=True)
parser.add_argument('-p', '--password', required=True)
parser.add_argument('-n', '--dry-run', action='store_true')
parser.add_argument('-y', '--yes', action='store_true')
parser.add_argument('-s', '--sub', nargs=2, action='append')
args = parser.parse_args()

client = gdata.contacts.client.ContactsClient(source='Mux-ContactsUpdater-1')
client.ClientLogin(args.user, args.password, client.source)

for contact in get_all_contacts(client):
    if contact.name is None:
        name = '(none)'
    else:
        name = contact.name.full_name.text

    updates = update_contact(contact, args.sub)
    if not updates:
        continue

    print('Updating contact "{0}" :'.format(name))
    for phone, old in updates:
        phone_type = phone_types.get(phone.rel, 'unknown')
        print('  {0} -> {1} ({2})'.format(old, phone.text, phone_type))
    if args.dry_run:
        continue

    if not args.yes:
        key = confirm('Really update (N/y/a) ? ', ('nN', 'yY', 'aA'), 'n')
        if key == 'n':
            continue
        if key == 'a':
            args.yes = True

    # We should handle Etags mismatch here, but since this is intended to
    # be used as an interactive application, it doesn't really matter.
    client.Update(contact)

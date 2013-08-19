#!/usr/bin/env python
# vi:ts=4:sw=4:et

# Update a Gandi zone using the XML-RPC API documented at:
#   http://doc.rpc.gandi.net/

from __future__ import print_function
import argparse
try:
    import xmlrpclib
except ImportError:
    import xmlrpc.client
import sys

parser = argparse.ArgumentParser()
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument('-k', '--api-key')
group.add_argument('-K', '--key-file')
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument('-z', '--zone-name')
group.add_argument('-i', '--zone-id', type=int)
parser.add_argument('name')
parser.add_argument('type')
parser.add_argument('value')

args = parser.parse_args()

if args.api_key is None:
    with open(args.key_file, "r") as f:
        args.api_key = f.readline().rstrip()

api = xmlrpclib.ServerProxy('https://rpc.gandi.net/xmlrpc/')

zones = api.domain.zone.list(args.api_key, {'public': False})

zone_id = None
for zone in zones:
    if (args.zone_name is not None and args.zone_name == zone['name']) or \
       (args.zone_id is not None and args.zone_id == zone['id']):
        zone_id = zone['id']
        break

if zone_id is None:
    print('Cannot find zone!', file=sys.stderr)
    sys.exit(1)

# Get records.
changed = False
records = api.domain.zone.record.list(args.api_key, zone_id, 0)
for record in records:
    # To get rid of at least 'id' keys which would cause the
    # api.domain.zone.record.set() call to fail.
    for key in record.keys():
        if key not in ('name', 'type', 'ttl', 'value'):
            del record[key]
    # Update record.
    if record['name'] == args.name and record['type'] == args.type:
        if record['value'] != args.value:
            record['value'] = args.value;
            changed = True

# Avoid creating a new zone version if nothing changed.
if not changed:
    sys.exit(0)

# Clone the currently active version.
version = api.domain.zone.version.new(args.api_key, zone_id, 0)

# Set modified records into new version.
api.domain.zone.record.set(args.api_key, zone_id, version, records)

# Activate new version.
success = api.domain.zone.version.set(args.api_key, zone_id, version)
if not success:
    print('Failed to set records!', file=sys.stderr)
    sys.exit(1)

print('Updated zone (version {0}) with record:'.format(version))
print('{0.name} {0.type} {0.value}'.format(args))

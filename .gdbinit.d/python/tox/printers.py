# -*- coding: iso-8859-1 -*-
# Pretty-printers for Tox.
import functools
import socket
import gdb.printing

## global variables
tab_size = 4
tab_spaces = ' '*tab_size

address_families = { 0:'AF_UNSPEC', 2: 'AF_INET', 10: 'AF_INET6', 130: 'TCP_INET', 138: 'TCP_INET6' }
net_packet = { 0:'NET_PACKET_PING_REQUEST', 1:'NET_PACKET_PING_RESPONSE', 2:'NET_PACKET_GET_NODES', \
               3:'NET_PACKET_SEND_NODES', 4:'NET_PACKET_SEND_NODES_IPV6', 16:'NET_PACKET_HANDSHAKE', \
               17:'NET_PACKET_SYNC', 18:'NET_PACKET_DATA', 24:'NET_PACKET_COOKIE_REQUEST', \
               25:'NET_PACKET_COOKIE_RESPONSE', 26:'NET_PACKET_CRYPTO_HS', 27:'NET_PACKET_CRYPTO_DATA', \
               32:'NET_PACKET_CRYPTO', 33:'NET_PACKET_LAN_DISCOVERY', 48:'NET_PACKET_GROUP_CHATS', \
               128:'NET_PACKET_ONION_SEND_INITIAL', 129:'NET_PACKET_ONION_SEND_1', 130:'NET_PACKET_ONION_SEND_2', \
               131:'NET_PACKET_ANNOUNCE_REQUEST', 132:'NET_PACKET_ANNOUNCE_RESPONSE',
               133:'NET_PACKET_ONION_DATA_REQUEST', 134:'NET_PACKET_ONION_DATA_RESPONSE', \
               140:'NET_PACKET_ONION_RECV_3', 141:'NET_PACKET_ONION_RECV_2', 142:'NET_PACKET_ONION_RECV_1' }
crypto_packet = { 32:'CRYPTO_PACKET_FRIEND_REQ', 48:'CRYPTO_PACKET_HARDENING', 254:'CRYPTO_PACKET_NAT_PING' }
crypto_packet_group_chat = { 48:'CRYPTO_PACKET_GROUP_CHAT_GET_NODES',  49:'CRYPTO_PACKET_GROUP_CHAT_GET_NODES', \
                             50:'CRYPTO_PACKET_GROUP_CHAT_BROADCAST' }


def is_zero(arr, len):
    return functools.reduce(lambda x,y: x and y, [arr[i]==0 for i in range(len)], True)

def array_to_hex_string(arr, len):
    return ''.join(['%02X'%arr[i] for i in range(len)])

def append_indent(string, indent = tab_spaces):
    return ('\n' + indent).join(string.split('\n'))

def clientlist_to_string(clientlist):
    ignore_zero_id = True
    clientlist_len = clientlist.type.sizeof // clientlist.dereference().type.sizeof
    items = map(lambda x: '[%d] = '%(x[0]) + append_indent(x[1], tab_spaces*2), \
                [(i,str(clientlist[i])) for i in range(clientlist_len) if not ignore_zero_id or not is_zero(clientlist['client_id'], 32)])
    return ('[%d] {'%(clientlist_len)) + (',\n' + tab_spaces).join(items) + ' }'


class ToxPrettyPrinter(gdb.printing.RegexpCollectionPrettyPrinter):

    def __call__(self, val):
        typename = val.type.name
        if not typename:
            return None

        for printer in self.subprinters:
            if printer.enabled and printer.compiled_re.search(typename):
                return printer.gen_printer(val)

        return None

class IPPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        string = self.val.type.name + ' { '
        string += 'family = %s' % address_families.get(int(self.val['family']), str(self.val['family']))
        if self.val['family'] == 10 or self.val['family'] == 138:
            string += ', ip6 = %s' % self.val['ip6']
        else:
            string += ', ip = %s' % self.val['ip4']
        return string + ' }'

class IPv4Printer:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        ip = self.val['uint8']
        return '%d.%d.%d.%d' % (ip[0], ip[1], ip[2], ip[3])

class IPv6Printer:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        string, ip = '', self.val['uint16']
        if ip[0] != 0:
            if ip[1] != 0:
                string = '%x:%x' % (ip[0], ip[1])
            else:
                string = '%s' % (ip[0],)
        elif ip[1] != 0:
            string = ':%s' % (ip[1],)
        for i in range(2, 8):
            if i == 7 or ip[i] != 0:
                string += '::%s'%int(ip[i]) if ip[i-1]==0 else ':%s'%int(ip[i])
        return string

class DHTPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '
        if self.val['net'] != 0:
            string += '\nnet = ' + append_indent(str(self.val['net'].dereference()))
        else:
            string += '\nnet = 0x0, '

        string += '\nclose_clientlist = ' + clientlist_to_string(self.val['close_clientlist'])
        string += '\nclose_lastgetnodes = ' + str(self.val['close_lastgetnodes']) +', '
        string += '\nclose_bootstrap_times = ' + str(self.val['close_bootstrap_times']) +', '
        string += '\nsecret_symmetric_key = ' + array_to_hex_string(self.val['secret_symmetric_key'], 32) +', '
        string += '\nself_public_key = ' + array_to_hex_string(self.val['self_public_key'], 32) +', '
        string += '\nself_secret_key = ' + array_to_hex_string(self.val['self_secret_key'], 32) +', '

        friends_list = self.val['friends_list']
        friends_list_len = int(self.val['num_friends'])
        string += '\nfriends_list = [%d] {' % friends_list_len
        for i in range(friends_list_len):
            string += ('\n%s[%d] = '%(tab, i)) + append_indent(str(friends_list[i]), tab*2)
        string += '}'

        #     Shared_Keys shared_keys_recv;
        #     Shared_Keys shared_keys_sent;

        #     struct PING   *ping;
        #     Ping_Array    dht_ping_array;
        #     Ping_Array    dht_harden_ping_array;
        if hasattr(self.val, 'assoc'):
            string += '\nassoc = ' + str(self.val['assoc']) if self.val['assoc'] != 0 else '0' + ', '
        string += '\nlast_run = ' + str(self.val['last_run']) + ', '

        cryptopackethandlers = self.val['cryptopackethandlers']
        string += '\ncryptopackethandlers = {'
        for i in range(cryptopackethandlers.type.sizeof // cryptopackethandlers.dereference().type.sizeof):
            if cryptopackethandlers[i]['function'] != 0:
                string += '\n%s[%s(%d)] = %s ' %(tab, crypto_packet.get(i, i), i, cryptopackethandlers[i])
        string += '}'

        return string

class NetworkingCorePrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '

        string += '\npackethandlers = {'
        packethandlers = self.val['packethandlers']
        for i in range(packethandlers.type.sizeof // packethandlers.dereference().type.sizeof):
            if packethandlers[i]['function'] != 0:
                string += '\n%s[%s(%d)] = %s ' %(tab, net_packet.get(i, i), i, packethandlers[i])
        string += '}, '

        string += '\nfamily = %s, ' % address_families.get(int(self.val['family']), str(self.val['family']))
        string += '\nport = %d, ' % socket.ntohs(self.val['port'])
        string += '\nsock = %d, ' % self.val['sock']
        string += '\nsend_fail_eagain = %d' % self.val['send_fail_eagain']

        return string + ' }'

class ClientDataPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '
        string += '\nclient_id = ' + array_to_hex_string(self.val['client_id'], 32) +', '
        string += '\nassoc4 = ' + append_indent(str(self.val['assoc4'])) +', '
        string += '\nassoc6 = ' + append_indent(str(self.val['assoc6']))
        return string + ' }'


class DHTFriendPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '
        string += '\nclient_id = ' + array_to_hex_string(self.val['client_id'], 32) +', '
        string += '\nclient_list = ' + clientlist_to_string(self.val['client_list'])
        string += '\nlastgetnode = ' + str(self.val['lastgetnode']) +', '
        string += '\nbootstrap_times = ' + str(self.val['bootstrap_times']) +', '
        string += '\nnat = ' + str(self.val['nat'])
        return string + ' }'

class HardeningPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '
        string += '\nroutes_requests_ok = ' + str(self.val['routes_requests_ok']) +', '
        string += '\nroutes_requests_timestamp = ' + str(self.val['routes_requests_timestamp']) +', '
        string += '\nroutes_requests_pingedid = ' + array_to_hex_string(self.val['routes_requests_pingedid'], 32) +', '
        string += '\nsend_nodes_ok = ' + str(self.val['send_nodes_ok']) +', '
        string += '\nsend_nodes_timestamp = ' + str(self.val['send_nodes_timestamp']) +', '
        string += '\nsend_nodes_pingedid = ' + array_to_hex_string(self.val['send_nodes_pingedid'], 32) +', '
        string += '\ntesting_requests = ' + str(self.val['testing_requests']) +', '
        string += '\ntesting_timestamp = ' + str(self.val['testing_timestamp']) +', '
        string += '\ntesting_pingedid = ' + array_to_hex_string(self.val['testing_pingedid'], 32)
        return string + ' }'

class IPPTsPngPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        tab = ' ' * tab_size
        string = self.val.type.name + ' { '
        string += '\nip_port = ' + str(self.val['ip_port']) +', '
        string += '\ntimestamp = ' + str(self.val['timestamp']) +', '
        string += '\nlast_pinged = ' + str(self.val['last_pinged']) +', '
        string += '\nhardening = ' + append_indent(str(self.val['hardening']), tab_spaces) +', '
        string += '\nret_ip_port = ' + str(self.val['ret_ip_port']) +', '
        string += '\nret_timestamp = ' + str(self.val['ret_timestamp'])
        return string + ' }'

# class Printer:
#     def __init__(self, val):
#         self.val = val

#     def to_string(self):
#         tab = ' ' * tab_size
#         string = self.val.type.name + ' { '
#         return string + ' }'


def build_pretty_printer():
    ## pp = gdb.printing.RegexpCollectionPrettyPrinter("toxcore library")
    pp = ToxPrettyPrinter("toxcore library")
    pp.add_printer('IP', '^IP$', IPPrinter)
    pp.add_printer('IPv4', '^IP4$', IPv4Printer)
    pp.add_printer('IPv6', '^IP6$', IPv6Printer)
    pp.add_printer('DHT', '^DHT$', DHTPrinter)
    pp.add_printer('Networking_Core', '^Networking_Core$', NetworkingCorePrinter)
    pp.add_printer('Client_data', '^Client_data$', ClientDataPrinter)
    pp.add_printer('DHT_Friend', '^DHT_Friend$', DHTFriendPrinter)
    pp.add_printer('Hardening', '^Hardening$', HardeningPrinter)
    pp.add_printer('IPPTsPng', '^IPPTsPng$', IPPTsPngPrinter)
    return pp


def register_tox_printers():
    import gdb.printing
    gdb.pretty_printers = []
    gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer())

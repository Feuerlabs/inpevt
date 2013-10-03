%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 2013-10-03 Magnus Feuer
%%%-------------------------------------------------------------------


%% Records used by the input event system: NEEDS WORK

-record(input_event,
        {
          port :: pid(), %% Port for the input event
	  sec  :: integer(), %% Seconds since epoch for event
	  usec :: integer(), %% usec siunce secont
	  type :: { syn | abs | rel | absinfo | key }, %% Type of event 
	  code_sym, %% symbol code FIXME! TIE INTO OTHER RECORDS
	  code_num, %% Numeric symbol
	  value %% Value of symbol
        }).

-record(syn, { element:: report | config | mt_report | mt_dropped }).


-record(key, { key, code::integer() }).


-record(rel, { element:: x | y | z | rx | ry | rz | hwheel | dial | wheel | misc }).


-record(absinfo, { value::integer(), minimum::integer(), maximum::integer(),
                   fuzz::integer(), flat::integer(), resolution::integer() }).


-record(abs, { element:: x | y | z | rx | ry | rz | throttle | rudder | wheel | gas |
                         brake | hat0x | hat0y | hat1x | hat1y | hat2x | hat2y | hat3x |
                         hat3y | pressure | distance | tilt_x | tilt_y | tool_width | volume | misc |
                         mt_slot | mt_touch_major | mt_touch_minor | mt_width_major | mt_width_minor |
                         mt_orientation | mt_position_x | mt_position_y | mt_tool_type | mt_blob_id |
                         mt_tracking_id | mt_pressure | mt_distance,  spec::#absinfo{} }).


-record(sw, { element:: lid | tablet_mode | headphone_insert | rfkill_all | radio | microphone_insert |
                        dock | lineout_insert | jack_physical_insert | videoout_insert |
                        camera_lens_cover | keypad_slide | front_proximity | rotate_lock |
                        linein_insert }).

-record(cap_spec, { element::#syn {} | #key {} | #rel {} | #abs {} | #sw {} }).

-record(drv_dev_id, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).

-record(descriptor, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).


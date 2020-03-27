// Copyright 2020 ETH Zurich and University of Bologna.
//
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

// Author: Florian Glaser <glaserf@iis.ee.ethz.ch>

module afe_thr_unit_type0 #(
  parameter W_CFG_ADDR  = 10,
  parameter W_AFE_DATA  = 16,
  parameter NUM_CH      = 8,
  parameter CH_ID_LSB   = 28,
  parameter CH_ID_WIDTH = 4 )
(
  // clock and reset
  input  logic        clk_i,
  input  logic        rstn_i,
 
  // cfg interface
  input  logic                   cfg_sel_i,
  input  logic                   cfg_wr_i,
  input  logic  [W_CFG_ADDR-1:0] cfg_addr_i,
  input  logic            [31:0] cfg_wdata_i,
  output logic            [31:0] cfg_rdata_o,
 
  // data from AFEs
  input  logic                   afe_data_vld_i,
  input  logic            [31:0] afe_data_i,

  // events out
  output logic      [NUM_CH-1:0] thr_events_o
);

  logic [NUM_CH-1:0][W_AFE_DATA-1:0]  cmp_val_lo_n, cmp_val_lo_q;
  logic [NUM_CH-1:0][W_AFE_DATA-1:0]  cmp_val_hi_n, cmp_val_hi_q;

  logic [NUM_CH-1:0]                  event_mask_lo_n, event_mask_lo_q;
  logic [NUM_CH-1:0]                  event_mask_hi_n, event_mask_hi_q;
  logic [NUM_CH-1:0]                  event_buf_lo_n, event_buf_lo_q;
  logic [NUM_CH-1:0]                  event_buf_hi_n, event_buf_hi_q;
  logic [NUM_CH-1:0]                  thr_events_n, thr_events_q;

  logic                               single_evt_n, single_evt_q;

  logic [NUM_CH-1:0]                  set_evt_lo, set_evt_hi;

  logic [CH_ID_WIDTH-1:0] ch_id;
  logic                   afe_vld_edge;
  logic             [2:0] afe_data_vld_sync;

  assign thr_events_o = thr_events_q;

  assign afe_vld_edge = afe_data_vld_sync[1] & ~afe_data_vld_sync[2];

  assign ch_id = afe_vld_edge ? afe_data_i[CH_ID_LSB+CH_ID_WIDTH-1:CH_ID_LSB] : '0;

  // in single event mode, an event is only output if none has happened since the last buffer read
  assign thr_events_n = single_evt_q ? ((~event_buf_lo_q & set_evt_lo) | (~event_buf_hi_q & set_evt_hi)) : (set_evt_lo | set_evt_hi);

  // write process
  always_comb begin
    cmp_val_hi_n    = cmp_val_hi_q;
    cmp_val_lo_n    = cmp_val_lo_q;
    event_mask_lo_n = event_mask_lo_q;
    event_mask_hi_n = event_mask_hi_q;
    single_evt_n    = single_evt_q;

    if (cfg_sel_i & cfg_wr_i) begin
      if (cfg_addr_i[6] == 1'b0) begin
        if (cfg_addr_i[5] == 1'b0)
          cmp_val_lo_n[cfg_addr_i[4:0]] = cfg_wdata_i[W_AFE_DATA-1:0];
        else
          cmp_val_hi_n[cfg_addr_i[4:0]] = cfg_wdata_i[W_AFE_DATA-1:0];
      end
      else begin 
        case (cfg_addr_i[2:0])
          3'h0: begin
            event_mask_lo_n  = cfg_wdata_i[NUM_CH-1:0];
            event_mask_hi_n  = cfg_wdata_i[NUM_CH+15:16];
          end
          3'h4: single_evt_n = cfg_wdata_i[0];
        endcase
      end
    end
  end

  // read process
  always_comb begin
    cfg_rdata_o     = '0;
    event_buf_lo_n  = event_buf_lo_q | set_evt_lo;
    event_buf_hi_n  = event_buf_hi_q | set_evt_hi;

    if (cfg_sel_i & ~cfg_wr_i) begin
      if (cfg_addr_i[6] == 1'b0) begin
        if (cfg_addr_i[5] == 1'b0)
          cfg_rdata_o[W_AFE_DATA-1:0] = cmp_val_lo_q[cfg_addr_i[4:0]];
        else
          cfg_rdata_o[W_AFE_DATA-1:0] = cmp_val_hi_q[cfg_addr_i[4:0]];
      end
      else begin 
        case (cfg_addr_i[2:0])
          3'h0: begin
            cfg_rdata_o[NUM_CH-1:0]    = event_mask_lo_q;
            cfg_rdata_o[NUM_CH+15:16]  = event_mask_hi_q;
          end
          3'h2: begin
            cfg_rdata_o[NUM_CH-1:0]    = event_buf_lo_q;
            cfg_rdata_o[NUM_CH+15:16]  = event_buf_hi_q;
            // clear event buffer, keep any incoming events
            event_buf_lo_n = set_evt_lo;
            event_buf_hi_n = set_evt_hi;
          end
          3'h4: cfg_rdata_o[0] = single_evt_q;
        endcase
      end
    end
  end

  // actual comparison
  always_comb begin
    set_evt_lo = '0;
    set_evt_hi = '0;

    if (afe_vld_edge & event_mask_lo_q[ch_id]) begin
      if ($signed(afe_data_i[W_AFE_DATA-1:0]) <= $signed(cmp_val_lo_q[ch_id]))
        set_evt_lo[ch_id] = 1'b1;
    end

    if (afe_vld_edge & event_mask_hi_q[ch_id]) begin
      if ($signed(afe_data_i[W_AFE_DATA-1:0]) >= $signed(cmp_val_hi_q[ch_id]))
        set_evt_hi[ch_id] = 1'b1;
    end
  end

  // register setup
  always_ff @(posedge clk_i, negedge rstn_i) begin
    if ( rstn_i == 1'b0 ) begin
      event_mask_lo_q   <= '0;
      event_mask_hi_q   <= '0;
      event_buf_lo_q    <= '0;
      event_buf_hi_q    <= '0;
      thr_events_q      <= '0;
      single_evt_q      <= 1'b0;
      afe_data_vld_sync <= '0;
    end
    else begin
      afe_data_vld_sync <= {afe_data_vld_sync[1:0],afe_data_vld_i};
      thr_events_q      <= thr_events_n;

      // register write activation condition
      if (cfg_sel_i) begin
        event_mask_lo_q <= event_mask_lo_n;
        event_mask_hi_q <= event_mask_hi_n;
        cmp_val_lo_q    <= cmp_val_lo_n;
        cmp_val_hi_q    <= cmp_val_hi_n;
        single_evt_q    <= single_evt_n;
      end

      if (cfg_sel_i | afe_vld_edge) begin
        event_buf_lo_q  <= event_buf_lo_n;
        event_buf_hi_q  <= event_buf_hi_n;
      end

    end
  end

endmodule

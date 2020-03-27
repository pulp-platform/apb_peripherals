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

module afe_thr_unit_type1 #(
  parameter W_CFG_ADDR      = 10,
  parameter W_AFE_DATA      = 16,
  parameter NUM_CH          = 8,
  parameter NUM_SUBCH       = 2,
  parameter NUM_THR_SETS    = 4,
  parameter CH_ID_LSB       = 28,
  parameter CH_ID_WIDTH     = 4,
  parameter SUBCH_ID_OFFS   = 0,
  parameter SUBCH_ID_LSB    = 26,
  parameter SUBCH_ID_WIDTH  = 2  )
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

  // events
  input  logic                   term_event_i,
  output logic                   thr_events_o
);

  localparam NUM_CH_LG        = $clog2(NUM_CH);
  localparam NUM_SUBCH_LG     = $clog2(NUM_SUBCH);
  localparam NUM_THR_SETS_LG  = $clog2(NUM_THR_SETS);

  logic [NUM_SUBCH-1:0][NUM_THR_SETS-1:0][W_AFE_DATA-1:0] cmp_val_lo_n, cmp_val_lo_q;
  logic [NUM_SUBCH-1:0][NUM_THR_SETS-1:0][W_AFE_DATA-1:0] cmp_val_hi_n, cmp_val_hi_q;
  logic                [NUM_THR_SETS-1:0][W_AFE_DATA-1:0] cmp_val_subch_lo, cmp_val_subch_hi;

  logic                 [NUM_CH-1:0][NUM_THR_SETS_LG-1:0] set_sel_n, set_sel_q;

  logic   [NUM_SUBCH-1:0][NUM_CH-1:0] event_mask_lo_n, event_mask_lo_q;
  logic   [NUM_SUBCH-1:0][NUM_CH-1:0] event_mask_hi_n, event_mask_hi_q;
  logic   [NUM_SUBCH-1:0][NUM_CH-1:0] event_buf_lo_n, event_buf_lo_q;
  logic   [NUM_SUBCH-1:0][NUM_CH-1:0] event_buf_hi_n, event_buf_hi_q;

  logic   [NUM_SUBCH-1:0][NUM_CH-1:0] set_evt_lo, set_evt_hi;

  logic                               thr_events_n, thr_events_q;
  logic                               chain_evt_n, chain_evt_q;
  logic                               term_evt_buf_n, term_evt_buf_q;

  logic             [CH_ID_WIDTH-1:0] ch_id_data;
  logic          [SUBCH_ID_WIDTH-1:0] subch_id_data;

  logic                               afe_vld_edge;
  logic                         [2:0] afe_data_vld_sync;

  assign thr_events_o  = thr_events_q;

  assign afe_vld_edge  = afe_data_vld_sync[1] & ~afe_data_vld_sync[2];

  assign ch_id_data    = afe_vld_edge ? afe_data_i[CH_ID_LSB    +: CH_ID_WIDTH] : '0;
  assign subch_id_data = afe_vld_edge ? afe_data_i[SUBCH_ID_LSB +: SUBCH_ID_WIDTH] - SUBCH_ID_OFFS : '0;

  assign cmp_val_subch_lo = (afe_vld_edge && (afe_data_i[SUBCH_ID_LSB +: SUBCH_ID_WIDTH] >= SUBCH_ID_OFFS)) ? cmp_val_lo_q[subch_id_data] : '0;
  assign cmp_val_subch_hi = (afe_vld_edge && (afe_data_i[SUBCH_ID_LSB +: SUBCH_ID_WIDTH] >= SUBCH_ID_OFFS)) ? cmp_val_hi_q[subch_id_data] : '0;

  // event generation
  always_comb begin
    thr_events_n = 1'b0;

    // in event chain mode, send an event out only after receiving the termination event
    if (chain_evt_q) begin
      if (term_evt_buf_n | term_evt_buf_q) thr_events_n = |(event_buf_lo_n | event_buf_lo_q) | |(event_buf_hi_n | event_buf_hi_q);
    end
    else begin
      // send an event every time an internal one has occurred
      thr_events_n = |(event_buf_lo_n & ~event_buf_lo_q) | |(event_buf_hi_n & ~event_buf_hi_q);
    end
  end

  // write process
  always_comb begin
    cmp_val_hi_n    = cmp_val_hi_q;
    cmp_val_lo_n    = cmp_val_lo_q;
    set_sel_n       = set_sel_q;
    event_mask_lo_n = event_mask_lo_q;
    event_mask_hi_n = event_mask_hi_q;
    chain_evt_n     = chain_evt_q;

    if (cfg_sel_i & cfg_wr_i) begin
      if (cfg_addr_i[6] == 1'b0) begin
        // this address partition allows up to 16 registers for low/high each for NUM_SUBCH*NUM_THR_SETS
        case (cfg_addr_i[5:4])
          2'b00: cmp_val_lo_n[cfg_addr_i[NUM_THR_SETS_LG +: NUM_SUBCH_LG]][cfg_addr_i[NUM_THR_SETS_LG-1:0]] = cfg_wdata_i[W_AFE_DATA-1:0];
          2'b01: cmp_val_hi_n[cfg_addr_i[NUM_THR_SETS_LG +: NUM_SUBCH_LG]][cfg_addr_i[NUM_THR_SETS_LG-1:0]] = cfg_wdata_i[W_AFE_DATA-1:0];
          2'b10: set_sel_n[cfg_addr_i[NUM_CH_LG-1:0]] = cfg_wdata_i[NUM_THR_SETS_LG-1:0];
        endcase
      end
      else begin 
        case (cfg_addr_i[5:3])
          3'b000: begin
            event_mask_lo_n[cfg_addr_i[NUM_SUBCH_LG-1:0]] = cfg_wdata_i[NUM_CH-1:0];
            event_mask_hi_n[cfg_addr_i[NUM_SUBCH_LG-1:0]] = cfg_wdata_i[NUM_CH+15:16];
          end
          3'b010: chain_evt_n = cfg_wdata_i[0];
        endcase
      end
    end
  end

  // read process
  always_comb begin
    cfg_rdata_o     = '0;
    event_buf_lo_n  = event_buf_lo_q | set_evt_lo;
    event_buf_hi_n  = event_buf_hi_q | set_evt_hi;
    term_evt_buf_n  = term_evt_buf_q | term_event_i;

    if (cfg_sel_i & ~cfg_wr_i) begin
      if (cfg_addr_i[6] == 1'b0) begin
        case (cfg_addr_i[5:4])
          2'b00: cfg_rdata_o[W_AFE_DATA-1:0]      = cmp_val_lo_q[cfg_addr_i[NUM_THR_SETS_LG+NUM_SUBCH_LG-1:NUM_THR_SETS_LG]][cfg_addr_i[NUM_THR_SETS_LG-1:0]];
          2'b01: cfg_rdata_o[W_AFE_DATA-1:0]      = cmp_val_hi_q[cfg_addr_i[NUM_THR_SETS_LG+NUM_SUBCH_LG-1:NUM_THR_SETS_LG]][cfg_addr_i[NUM_THR_SETS_LG-1:0]];
          2'b10: cfg_rdata_o[NUM_THR_SETS_LG-1:0] = set_sel_q[cfg_addr_i[NUM_CH_LG-1:0]];
        endcase
      end
      else begin 
        case (cfg_addr_i[5:3])
          3'b000: begin
            cfg_rdata_o[NUM_CH-1:0]    = event_mask_lo_q[cfg_addr_i[NUM_SUBCH_LG-1:0]];
            cfg_rdata_o[NUM_CH+15:16]  = event_mask_hi_q[cfg_addr_i[NUM_SUBCH_LG-1:0]];
          end
          3'b001: begin
            cfg_rdata_o[NUM_CH-1:0]    = event_buf_lo_q[cfg_addr_i[NUM_SUBCH_LG-1:0]];
            cfg_rdata_o[NUM_CH+15:16]  = event_buf_hi_q[cfg_addr_i[NUM_SUBCH_LG-1:0]];
            // clear event buffer, keep any incoming events
            event_buf_lo_n[cfg_addr_i[NUM_SUBCH_LG-1:0]] = set_evt_lo[cfg_addr_i[NUM_SUBCH_LG-1:0]];
            event_buf_hi_n[cfg_addr_i[NUM_SUBCH_LG-1:0]] = set_evt_hi[cfg_addr_i[NUM_SUBCH_LG-1:0]];
            term_evt_buf_n = term_event_i;
          end
          3'b010: cfg_rdata_o[0] = chain_evt_q;
        endcase
      end
    end
  end

  // actual comparison
  always_comb begin
    set_evt_lo = '0;
    set_evt_hi = '0;

    if (afe_vld_edge && (afe_data_i[SUBCH_ID_LSB +: SUBCH_ID_WIDTH] >= SUBCH_ID_OFFS)) begin
      if (event_mask_lo_q[subch_id_data][ch_id_data]) begin
        if ($signed(afe_data_i[W_AFE_DATA-1:0]) <= $signed(cmp_val_subch_lo[set_sel_q[ch_id_data]]))
          set_evt_lo[subch_id_data][ch_id_data] = 1'b1;
      end

      if (event_mask_hi_q[subch_id_data][ch_id_data]) begin
        if ($signed(afe_data_i[W_AFE_DATA-1:0]) >= $signed(cmp_val_subch_hi[set_sel_q[ch_id_data]]))
          set_evt_hi[subch_id_data][ch_id_data] = 1'b1;
      end
    end
  end

  // register setup
  always_ff @(posedge clk_i, negedge rstn_i) begin
    if ( rstn_i == 1'b0 ) begin
      event_mask_lo_q   <= '0;
      event_mask_hi_q   <= '0;
      event_buf_lo_q    <= '0;
      event_buf_hi_q    <= '0;
      set_sel_q         <= '0;
      chain_evt_q       <= 1'b1;
      thr_events_q      <= 1'b0;
      term_evt_buf_q    <= 1'b0;
      afe_data_vld_sync <= '0;
    end
    else begin
      afe_data_vld_sync <= {afe_data_vld_sync[1:0],afe_data_vld_i};
      term_evt_buf_q    <= term_evt_buf_n;
      thr_events_q      <= thr_events_n;

      // register write activation condition
      if (cfg_sel_i) begin
        event_mask_lo_q <= event_mask_lo_n;
        event_mask_hi_q <= event_mask_hi_n;
        cmp_val_lo_q    <= cmp_val_lo_n;
        cmp_val_hi_q    <= cmp_val_hi_n;
        set_sel_q       <= set_sel_n;
        chain_evt_q     <= chain_evt_n;
      end

      if (cfg_sel_i | afe_vld_edge) begin
        event_buf_lo_q  <= event_buf_lo_n;
        event_buf_hi_q  <= event_buf_hi_n;
      end

    end
  end

endmodule

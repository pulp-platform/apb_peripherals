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


module apb_threshold_unit #(
  parameter W_APB_ADDR    = 16,
  parameter W_UNIT_ADDR   = 10,
  parameter W_UNIT_SEL    = 3,

  parameter NUM_AFE       = 4,

  parameter AFE_THR_N_TERM_EVT = 1,
  parameter AFE_THR_N_EVT_TOT  = 4,

  parameter int AFE_THR_MASK         [NUM_AFE-1:0] = '{default:  0},
  parameter int AFE_THR_TYPE         [NUM_AFE-1:0] = '{default:  0},

  parameter int AFE_THR_DATA_W       [NUM_AFE-1:0] = '{default: 16},
  parameter int AFE_THR_NUM_CHS      [NUM_AFE-1:0] = '{default:  8},
  parameter int AFE_THR_NUM_SUBCHS   [NUM_AFE-1:0] = '{default:  0},
  parameter int AFE_THR_SUBCHID_OFFS [NUM_AFE-1:0] = '{default:  0},
  parameter int AFE_THR_NUM_SETS     [NUM_AFE-1:0] = '{default:  1},
  parameter int AFE_THR_NUM_EVTS     [NUM_AFE-1:0] = '{default:  1},
  parameter int AFE_THR_OFFS_EVTS    [NUM_AFE-1:0] = '{default:  1},
  parameter int AFE_THR_TERM_EVT_IDX [NUM_AFE-1:0] = '{default:  0},
  parameter int AFE_RX_CHID_LSB      [NUM_AFE-1:0] = '{default: 28},
  parameter int AFE_RX_CHID_WIDTH    [NUM_AFE-1:0] = '{default:  4},
  parameter int AFE_RX_SUBCHID_LSB   [NUM_AFE-1:0] = '{default: 26},
  parameter int AFE_RX_SUBCHID_WIDTH [NUM_AFE-1:0] = '{default:  2} )
(
  // clock and reset
  input  logic        clk_i,
  input  logic        rstn_i,
 
  // APB interface
  input  logic                          apb_sel_i,
  input  logic                          apb_en_i,
  input  logic                          apb_write_i,
  output logic                          apb_ready_o,
  output logic                          apb_slverr_o,
  input  logic         [W_APB_ADDR-1:0] apb_address_i,
  input  logic                   [31:0] apb_wdata_i,
  output logic                   [31:0] apb_rdata_o,
 
  // data from AFEs
  input  logic      [NUM_AFE-1:0]       afe_data_vld_i,
  input  logic      [NUM_AFE-1:0][31:0] afe_data_i,

  // events
  input  logic [AFE_THR_N_TERM_EVT-1:0] term_events_i,
  output logic  [AFE_THR_N_EVT_TOT-1:0] thr_events_o
);

  logic [NUM_AFE-1:0]       unit_select;
  logic [NUM_AFE-1:0][31:0] unit_rdata;

  assign apb_slverr_o = 1'b0;
  assign apb_ready_o  = 1'b1;


  always_comb begin
    apb_rdata_o = '0;
    unit_select = '0;

    if (apb_en_i & apb_sel_i & apb_write_i) begin
      // write access, decode AFE select
      unit_select[apb_address_i[W_UNIT_SEL+W_UNIT_ADDR-1:W_UNIT_ADDR]] = 1'b1;
    end
    else if (apb_en_i & apb_sel_i & ~apb_write_i) begin
      // read access
      unit_select[apb_address_i[W_UNIT_SEL+W_UNIT_ADDR-1:W_UNIT_ADDR]] = 1'b1;
      apb_rdata_o = unit_rdata[apb_address_i[W_UNIT_SEL+W_UNIT_ADDR-1:W_UNIT_ADDR]];
    end
  end


  for (genvar gen_unit=0; gen_unit<NUM_AFE; gen_unit++) begin : THR_UNIT_GEN
    if (AFE_THR_MASK[gen_unit] == 1) begin : THR_UNIT_MASK
      case (AFE_THR_TYPE[gen_unit])
        0: begin : THR_UNIT_INST
          afe_thr_unit_type0 #(
            .W_CFG_ADDR   ( W_UNIT_ADDR-2               ),
            .W_AFE_DATA   ( AFE_THR_DATA_W[gen_unit]    ),
            .NUM_CH       ( AFE_THR_NUM_CHS[gen_unit]   ),
            .CH_ID_LSB    ( AFE_RX_CHID_LSB[gen_unit]   ),
            .CH_ID_WIDTH  ( AFE_RX_CHID_WIDTH[gen_unit] ) )
          afe_thr_unit_i (
            .clk_i          ( clk_i                     ),
            .rstn_i         ( rstn_i                    ),

            .cfg_sel_i      ( unit_select[gen_unit]     ),
            .cfg_addr_i     ( apb_address_i[W_UNIT_ADDR-1:2] ),
            .cfg_wr_i       ( apb_write_i               ),
            .cfg_wdata_i    ( apb_wdata_i               ),
            .cfg_rdata_o    ( unit_rdata[gen_unit]      ),

            .thr_events_o   ( thr_events_o[AFE_THR_OFFS_EVTS[gen_unit]+AFE_THR_NUM_EVTS[gen_unit]-1:AFE_THR_OFFS_EVTS[gen_unit]] ),

            .afe_data_i     ( afe_data_i[gen_unit]      ),
            .afe_data_vld_i ( afe_data_vld_i[gen_unit]  )
          );
        end

        1: begin : THR_UNIT_INST
          afe_thr_unit_type1 #(
            .W_CFG_ADDR     ( W_UNIT_ADDR-2                  ),
            .W_AFE_DATA     ( AFE_THR_DATA_W[gen_unit]       ),
            .NUM_CH         ( AFE_THR_NUM_CHS[gen_unit]      ),
            .NUM_SUBCH      ( AFE_THR_NUM_SUBCHS[gen_unit]   ),
            .NUM_THR_SETS   ( AFE_THR_NUM_SETS[gen_unit]     ),
            .CH_ID_LSB      ( AFE_RX_CHID_LSB[gen_unit]      ),
            .CH_ID_WIDTH    ( AFE_RX_CHID_WIDTH[gen_unit]    ),
            .SUBCH_ID_LSB   ( AFE_RX_SUBCHID_LSB[gen_unit]   ),
            .SUBCH_ID_WIDTH ( AFE_RX_SUBCHID_WIDTH[gen_unit] ),
            .SUBCH_ID_OFFS  ( AFE_THR_SUBCHID_OFFS[gen_unit] ) )
          afe_thr_unit_i (
            .clk_i          ( clk_i                     ),
            .rstn_i         ( rstn_i                    ),

            .cfg_sel_i      ( unit_select[gen_unit]     ),
            .cfg_addr_i     ( apb_address_i[W_UNIT_ADDR-1:2] ),
            .cfg_wr_i       ( apb_write_i               ),
            .cfg_wdata_i    ( apb_wdata_i               ),
            .cfg_rdata_o    ( unit_rdata[gen_unit]      ),

            .term_event_i   ( term_events_i[AFE_THR_TERM_EVT_IDX[gen_unit]] ),
            .thr_events_o   ( thr_events_o[AFE_THR_OFFS_EVTS[gen_unit]+AFE_THR_NUM_EVTS[gen_unit]-1:AFE_THR_OFFS_EVTS[gen_unit]] ),

            .afe_data_i     ( afe_data_i[gen_unit]      ),
            .afe_data_vld_i ( afe_data_vld_i[gen_unit]  )
          );
        end

        2: begin : THR_UNIT_INST
          afe_thr_unit_type2 #(
            .W_CFG_ADDR   ( W_UNIT_ADDR-2               ),
            .W_AFE_DATA   ( AFE_THR_DATA_W[gen_unit]    ),
            .NUM_CH       ( AFE_THR_NUM_SETS[gen_unit]  ),
            .CH_ID_LSB    ( AFE_RX_CHID_LSB[gen_unit]   ),
            .CH_ID_WIDTH  ( AFE_RX_CHID_WIDTH[gen_unit] ) )
          afe_thr_unit_i (
            .clk_i          ( clk_i                     ),
            .rstn_i         ( rstn_i                    ),

            .cfg_sel_i      ( unit_select[gen_unit]     ),
            .cfg_addr_i     ( apb_address_i[W_UNIT_ADDR-1:2] ),
            .cfg_wr_i       ( apb_write_i               ),
            .cfg_wdata_i    ( apb_wdata_i               ),
            .cfg_rdata_o    ( unit_rdata[gen_unit]      ),

            .thr_events_o   ( thr_events_o[AFE_THR_OFFS_EVTS[gen_unit]+AFE_THR_NUM_EVTS[gen_unit]-1:AFE_THR_OFFS_EVTS[gen_unit]] ),

            .afe_data_i     ( afe_data_i[gen_unit]      ),
            .afe_data_vld_i ( afe_data_vld_i[gen_unit]  )
          );
        end
      endcase
    end
    else begin : THR_UNIT_DUMMY
      assign unit_rdata[gen_unit] = '0;
    end
  end


  // register setup
  /* no register at top-level scope at this time
  always_ff @(posedge clk_i, negedge rstn_i) begin
    if ( rstn_i == 1'b0 ) begin
      
    end
    else begin
      
    end
  end
  */

endmodule

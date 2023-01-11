# To run this makefile, make sure you have a local data.txt that holds 
# the directory path to the data

# ==============================================================================
# establish directory paths
# ==============================================================================
# load in a local .txt file that holds the directory path to the data
ifeq ($(OS), Windows_NT)
	DATA = $(file < data.txt)
else
	DATA = $(shell cat data.txt)
endif

DIR = ${CURDIR}

RDIR = $(DIR)/code
TDIR = $(DIR)/output/tables
FDIR = $(DIR)/output/figures
EDIR = $(DIR)/output/estimates
PDIR = $(DIR)/output/press

RDIR_clean = $(RDIR)/Data_Cleaning
RDIR_analysis = $(RDIR)/Analysis
RDIR_functions = $(RDIR)/functions

# data files
DATA_gen = $(DATA)/generated_data
DATA_gen_shape = $(DATA)/generated_shape_files
DATA_int = $(DATA)/intermediate_data
DATA_raw = $(DATA)/raw_data
DATA_shape = $(DATA)/shape_files

# Name of final files
DATA_final = $(DATA_gen)/final_leases.Rda
PAPER = $(DIR)/writeups/cs_texas
APPENDIX = $(DIR)/writeups/cs_texas_appendix

.PHONY: install getdata clean
# make clean is useful for getting rid of temporary latex files and doing a 
# build of the various documents from scratch
all: $(PAPER).pdf
clean: 
	rm $(DIR)/writeups/*.aux
	rm $(DIR)/writeups/*.bbl
	rm $(DIR)/writeups/*.blg
	rm $(DIR)/writeups/*.log
	rm $(DIR)/writeups/*.out
	rm $(DIR)/writeups/*.pdf
install:
	Rscript $(RDIR)/package_installation.R
getdata:
	Rscript $(RDIR)/get_replication_data.R

# ==============================================================================
# Check that PSF data files exist if not, make skips this section
# If user does not have PSF data, make sure the repo has the following files:
# output/tables/parcel_balance.tex
# output/tables/summary_stats_parcel.tex
# output/figures/active_plot.png
# output/tables/parcel_regressions.tex
# output/tables/lease_parcel_comparisons_linear.tex
# output/tables/lease_parcel_comparisons_poisson.tex
# output/tables/logrank_stats.tex
# output/tables/spell_stats.tex
# output/figures/ipwkm10.png
# output/figures/ipwkm20.png

# also, make sure the generated data folder *already* includes
# $(DATA_gen)/leases_spatial_info.Rda
# $(DATA_gen)/parcel_lease_bridge.Rda
# ==============================================================================
psf := $(wildcard $(DATA_shape)/PSF/Chicago_PSF_180207.gdb/*)
lc := $(wildcard $(DATA_shape)/Land_Cover/landcover/*)
lc_info := $(wildcard $(DATA_shape)/Land_Cover/info/*)

ifneq ($(psf), )
$(DATA_gen_shape)/psf.Rda: \
	$(RDIR_clean)/psf_conversion.R \
	$(psf)
	Rscript $<

$(TDIR)/parcel_balance.tex \
	$(TDIR)/summary_stats_parcel.tex: \
	$(RDIR_analysis)/parcel_stats.R \
	$(DATA_gen)/clean_parcels.Rda \
	$(DATA_gen)/final_parcel_outcomes.Rda
	Rscript $<

$(DATA_gen)/clean_parcels.Rda: \
	$(RDIR_clean)/clean_parcels.R \
	$(DATA_gen_shape)/psf.Rda \
	$(DATA_gen_shape)/texas_grid5.Rda \
	$(DATA_gen_shape)/texas_grid10.Rda \
	$(DATA_gen_shape)/texas_grid20.Rda \
	$(DATA_gen_shape)/roads.Rda \
	$(DATA_gen_shape)/waterbodies.Rda \
	$(DATA_gen_shape)/river_streams.Rda \
	$(DATA_gen_shape)/shale_plays.Rda \
	$(DATA_gen_shape)/isopach_permian_interpolate.Rda \
	$(DATA_gen_shape)/isopach_eagle_ford_interpolate.Rda \
	$(lc) $(lc_info)
	Rscript $<

$(DATA_gen)/parcel_lease_bridge.Rda: \
	$(RDIR_clean)/clean_parcel_bridge.R \
	$(DATA_raw)/leases/LeaseLandFile.csv \
	$(DATA_raw)/leases/lease_parcel_scraping/scraped_parcels.Rda \
	$(DATA_gen_shape)/psf.Rda \
	$(DATA_raw)/leases/June2020/OAG_Leases_Active.shp \
	$(DATA_raw)/leases/June2020/OAG_Leases_Inactive.shp
	Rscript $<

$(DATA_gen)/leases_spatial_info.Rda: \
	$(RDIR_clean)/clean_lease_spatial_info.R \
	$(DATA_gen)/parcel_lease_bridge.Rda \
	$(DATA_gen)/clean_parcels.Rda
	Rscript $<

$(DATA_gen)/lease_parcels_monthly_outcomes.Rda $(DATA_gen)/lease_orders.Rda: \
	$(RDIR_analysis)/make_lease_parcels_monthly.R \
	$(DATA_gen)/parcel_lease_bridge.Rda \
	$(DATA_gen)/clean_parcels.Rda \
	$(DATA_gen)/clean_leases.Rda \
	$(DATA_gen)/clean_lease_outcomes_monthly.Rda
	Rscript $<

$(DATA_gen)/final_parcel_outcomes.Rda $(DATA_gen)/outcomes_earlyLeases.Rda \
	$(DATA_gen)/outcomes_earlyLeases_earlydates.Rda: \
	$(RDIR_analysis)/final_parcel_outcomes.R \
	$(DATA_gen)/clean_parcels.Rda \
	$(DATA_gen)/clean_leases.Rda \
	$(DATA_gen)/lease_parcels_monthly_outcomes.Rda \
	$(DATA_gen)/lease_orders.Rda
	Rscript $<

$(FDIR)/active_plot.png: \
	$(RDIR_analysis)/parcel_active_plot.R \
	$(DATA_gen)/outcomes_earlyLeases.Rda
	Rscript $<

$(TDIR)/parcel_regressions.tex \
	$(TDIR)/lease_parcel_comparisons_linear.tex \
	$(TDIR)/lease_parcel_comparisons_poisson.tex: \
	$(RDIR_analysis)/regressions_parcels.R \
	$(DATA_gen)/final_parcel_outcomes.Rda \
	$(DATA_gen)/final_leases.Rda \
	$(DATA_gen)/outcomes_earlyLeases.Rda
	Rscript $<

$(TDIR)/logrank_stats.tex $(TDIR)/spell_stats.tex \
	$(FDIR)/ipwkm10.png \
	$(FDIR)/ipwkm20.png: \
	$(RDIR_analysis)/parcel_hazard_analysis.R \
	$(DATA_gen)/outcomes_earlyLeases_earlydates.Rda
	Rscript $<

endif

# ==============================================================================
# Read in non-lease and non-parcel shapefiles and save as .Rda files
# ==============================================================================
isopach := $(wildcard $(DATA_shape)/TightOil_ShaleGas_IndividualPlays_Lower48_EIA/*Isopach*.shp)
$(DATA_gen_shape)/roads.Rda \
	$(DATA_gen_shape)/waterbodies.Rda \
	$(DATA_gen_shape)/river_streams.Rda \
	$(DATA_gen_shape)/shale_plays.Rda \
	$(DATA_gen_shape)/isopach_permian_interpolate.Rda \
	$(DATA_gen_shape)/isopach_eagle_ford_interpolate.Rda \
	$(DATA_gen_shape)/texas_grid5.Rda \
	$(DATA_gen_shape)/texas_grid10.Rda \
	$(DATA_gen_shape)/texas_grid20.Rda: \
	$(RDIR_clean)/rda_conversion.R \
	$(DATA_shape)/txdot-roads_tx/txdot-2015-roadways_tx.shp \
	$(DATA_shape)/usgs-rivers_tx/Waterbodies/Waterbodies.shp \
	$(DATA_shape)/usgs-rivers_tx/Rivers_Streams/Rivers_Streams.shp \
	$(isopach) \
	$(DATA_shape)/TightOil_ShalePlays_US_EIA_Jan2019/TightOil_ShalePlays_US_EIA_Jan2019.shp \
	$(DATA_shape)/texas_grids_bounding_box/texas_grids_bounding_box.shp
	Rscript $<

# ==============================================================================
# clean auction data: bids and notices
# ==============================================================================
bids := $(wildcard $(DATA_raw)/bids/Final_GLO*)
$(DATA_gen)/clean_bids.Rda $(DATA_gen)/clean_notices.Rda: \
	$(RDIR_clean)/clean_auctions.R \
	$(bids) \
	$(DATA_raw)/bids/newest_bids.xlsx \
	$(DATA_raw)/bids/old_bids.xlsx \
	$(DATA_int)/glo_notices_final.csv \
	$(DATA_raw)/notices/old_notices.xlsx \
	$(DATA_raw)/notices/newest_notices.xlsx \
	$(DATA_int)/manually_improved_names.xlsx 
	Rscript $<

# ==============================================================================
# clean lease related files
# assignments, coversheets, payment data
# the lease data itself
# and finally lease level monthly outcomes
# ==============================================================================
$(DATA_gen)/assignments.Rda: \
	$(RDIR_clean)/clean_assignments.R \
	$(DATA_raw)/Assignments/glo_assignments.csv \
	$(DATA_int)/assignments/partial_assignments.csv \
	$(DATA_int)/assignments/manual_pass.xlsx \
	$(DATA_int)/assignments/glo_assignments_fix.csv
	Rscript $<

$(DATA_gen)/recommended_rentals.Rda \
	$(DATA_gen)/coversheets.Rda: \
	$(RDIR_clean)/clean_coversheets.R \
	$(DATA_raw)/coversheets/Final_Term_Sheet.xlsx \
	$(DATA_raw)/coversheets/Highlighted_Terms.xlsx
	Rscript $<

payments := $(wildcard $(DATA_raw)/payments/Royalty*)
$(DATA_gen)/glo_royalties.Rda $(DATA_gen)/glo_royalties_lifo_flag.Rda : \
	$(RDIR_clean)/clean_glo_production_revs.R \
	$(payments)
	Rscript $<

$(DATA_gen)/glo_rentals_bonuses.Rda $(DATA_gen)/uiflags.Rda: \
	$(RDIR_clean)/clean_glo_rentals_bonuses.R \
	$(DATA_raw)/payments/Rental_Payments_R2045515_PIR20-0640.xlsx
	Rscript $<

$(DATA_gen)/prices.Rda: $(RDIR_clean)/clean_prices.R \
	$(DATA_raw)/prices/RNGWHHDm.xls \
	$(DATA_raw)/prices/RWTCm.xls
	Rscript $<

$(DATA_gen)/clean_leases.Rda $(DATA_gen)/clean_leases_shapes.Rda: \
	$(RDIR_clean)/clean_leases.R \
	$(DATA_raw)/leases/June2020/OAG_Leases_Active.shp \
	$(DATA_raw)/leases/June2020/OAG_Leases_Inactive.shp \
	$(DATA_raw)/leases/tblMineralLeaseSummaryAll_2020.csv \
	$(DATA_int)/leases/missing_bonus.csv \
	$(DATA_int)/leases/missing_bonus2.csv \
	$(DATA_int)/leases/missing_bonus3.xlsx \
	$(DATA_int)/leases/lease_check.csv \
	$(DATA_int)/leases/missing_undivided.xlsx \
	$(DATA_gen)/uiflags.Rda \
	$(DATA_gen)/coversheets.Rda \
	$(DATA_gen)/clean_bids.Rda \
	$(DATA_int)/leases/missing_bonus4.xlsx \
	$(DATA_int)/leases/royalty_fixes.xlsx \
	$(DATA_int)/leases/effective_date_fixes.xlsx \
	$(DATA_int)/leases/term_fixes.xlsx \
	$(DATA_gen)/parcel_lease_bridge.Rda \
	$(DATA_gen)/leases_spatial_info.Rda \
	$(DATA_gen)/assignments.Rda \
	$(DATA_int)/manually_improved_names.xlsx 
	Rscript $<

$(DATA_gen)/clean_lease_outcomes_monthly.Rda: \
	$(RDIR_clean)/clean_lease_outcomes_monthly.R \
	$(DATA_gen)/clean_leases.Rda \
	$(DATA_gen)/prices.Rda \
	$(DATA_gen)/glo_royalties.Rda \
	$(DATA_gen)/glo_rentals_bonuses.Rda \
	$(DATA_raw)/leases/odas.csv
	Rscript $<

# ===========================================================================
# Create Lease Analysis Dataset
# ===========================================================================
$(DATA_final) $(DATA_gen)/probfirms_map.Rda: \
	$(RDIR_analysis)/final_leases.R \
	$(DATA_gen)/coversheets.Rda \
	$(DATA_raw)/addenda.csv \
	$(DATA_gen)/clean_leases_shapes.Rda \
	$(DATA_gen)/clean_lease_outcomes_monthly.Rda \
	$(DATA_gen)/glo_royalties_lifo_flag.Rda \
	$(DATA_gen)/clean_leases.Rda
	Rscript $<

# ===========================================================================
# Lease-based Figures and Tables Output
# ===========================================================================
$(TDIR)/allocative.tex: $(RDIR_analysis)/allocative_diffs.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/auction_number_bids.tex $(TDIR)/TopPairAuctionShares.tex \
	$(TDIR)/auction_bonus_regressions.tex $(TDIR)/auction_appendix.tex \
	$(EDIR)/tract_date_leased.tex \
	$(EDIR)/tract_date_leased_insample.tex \
	$(EDIR)/tract_date_leased_share.tex \
	$(EDIR)/tract_ever_leased_share.tex \
	$(EDIR)/ever_bidder_negotiation_share.tex \
	$(EDIR)/negotiation_winner_auction_share.tex \
	$(EDIR)/drop_bidder_v_21.tex \
	$(EDIR)/drop_bidder_v_32.tex \
	$(EDIR)/drop_bidder_v_31.tex: \
	$(RDIR_analysis)/auction_analysis.R \
	$(DATA_final) \
	$(DATA_gen)/clean_bids.Rda \
	$(DATA_gen)/clean_notices.Rda
	Rscript $<

$(EDIR)/Nsample_RAL_RAW.tex $(EDIR)/Nsample_STATE_RAW.tex \
	$(EDIR)/Nsample_NEGOTIATION_CLEAN.tex \
	$(EDIR)/Nsample_AUCTION_CLEAN.tex \
	$(FDIR)/cohorts.png \
	$(EDIR)/Nsample_AUCTION_NOTHICK.tex \
	$(EDIR)/Nsample_NEGOTIATION_NOTHICK.tex \
	$(TDIR)/summary_stats_by_type.tex \
	$(FDIR)/summary_data_construction.tex: \
	$(RDIR_analysis)/lease_stats.R \
	$(DATA_final)
	Rscript $<

$(FDIR)/glo_leases_in_texas.png $(FDIR)/sample_glo_leases.png: \
	$(RDIR_analysis)/leases_maps.R \
	$(DATA_final) \
	$(DATA_gen)/clean_leases_shapes.Rda \
	$(DATA_shape)/us_county/cb_2015_us_county_500k.shp \
	$(DATA_shape)/TightOil_ShalePlays_US_EIA_Jan2019/TightOil_ShalePlays_US_EIA_Jan2019.shp \
	$(DATA_gen_shape)/texas_grid10.Rda
	Rscript $<

$(TDIR)/stacked_regressions_addenda.tex: \
	$(RDIR_analysis)/regressions_bonus_raladdenda.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/lease_regressions_extra_bonus.tex \
	$(TDIR)/lease_regressions_extra_output.tex: \
	$(RDIR_analysis)/regressions_extracontrols.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/firms_regressions.tex: $(RDIR_analysis)/regressions_firms.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/bonus_regressions.tex $(TDIR)/bonus_regressions_discussion.tex \
	$(TDIR)/logbonus_regressions.tex \
	$(TDIR)/logbonus_regressions_discussion.tex \
	$(TDIR)/royalty_term_regressions.tex \
	$(EDIR)/negotiation_avg_bonus.tex \
	$(EDIR)/Bonus_Grid10_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log_total.tex: \
	$(RDIR_analysis)/regressions_lease_contracts.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/logbonus_regressions_lessor_heterogeneity.tex: \
	$(RDIR_analysis)/regressions_lessors.R \
	$(DATA_final)
	Rscript $<

# output regressions
$(TDIR)/stacked_output_levels.tex \
	$(TDIR)/stacked_output_poisson.tex \
	$(EDIR)/negotiation_avg_dboe.tex \
	$(EDIR)/negotiation_avg_revenue.tex \
	$(EDIR)/Poisson_LeaseRevenue_Grid10Yr.tex \
	$(EDIR)/SellerRevenue_Grid10Yr_total.tex: \
	$(RDIR_analysis)/regressions_outputs.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/logbonus_size_heterogeneity.tex $(EDIR)/median_negotiated_size.tex: \
	$(RDIR_analysis)/size_het.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/drilled_regressions.tex $(TDIR)/logdboe_drilled_regressions.tex: \
	$(RDIR_analysis)/regressions_drilled.R \
	$(DATA_final)
	Rscript $<

# ===========================================================================
# Function dependencies
# ===========================================================================
$(RDIR_clean)/clean_assignments.R $(RDIR_clean)/clean_auctions.R \
	$(RDIR_clean)/clean_coversheets.R \
	$(RDIR_clean)/clean_glo_production_revs.R \
	$(RDIR_clean)/clean_glo_rentals_bonuses.R \
	$(RDIR_clean)/clean_lease_outcomes_monthly.R \
	$(RDIR_clean)/clean_lease_spatial_info.R \
	$(RDIR_clean)/clean_leases.R \
	$(RDIR_clean)/clean_parcel_bridge.R \
	$(RDIR_clean)/clean_parcels.R \
	$(RDIR_clean)/clean_prices.R \
	$(RDIR_clean)/psf_conversion.R \
	$(RDIR_clean)/rda_conversion.R \
	$(RDIR_analysis)/allocative_diffs.R \
	$(RDIR_analysis)/auction_analysis.R \
	$(RDIR_analysis)/final_leases.R \
	$(RDIR_analysis)/leases_maps.R \
	$(RDIR_analysis)/lease_stats.R \
	$(RDIR_analysis)/make_lease_parcels_monthly.R \
	$(RDIR_analysis)/parcel_stats.R \
	$(RDIR_analysis)/parcel_monthplots.R \
	$(RDIR_analysis)/regressions_bonus_raladdenda.R \
	$(RDIR_analysis)/regressions_extracontrols.R \
	$(RDIR_analysis)/regressions_firms.R \
	$(RDIR_analysis)/regressions_lease_contracts.R \
	$(RDIR_analysis)/regressions_lessors.R \
	$(RDIR_analysis)/regressions_outputs.R \
	$(RDIR_analysis)/regressions_parcels.R \
	$(RDIR_analysis)/size_het.R: \
	$(RDIR)/functions/utils.R \
	$(RDIR)/functions/latex_number.R \
	$(RDIR)/texas_constants.R \
	$(RDIR)/paths.R

$(RDIR_analysis)/auction_analysis.R \
	$(RDIR_analysis)/parcel_stats.R \
	$(RDIR_analysis)/parcel_monthplots.R \
	$(RDIR_analysis)/regressions_bonus_raladdenda.R \
	$(RDIR_analysis)/regressions_lease_contracts.R \
	$(RDIR_analysis)/regressions_firms.R \
	$(RDIR_analysis)/regressions_extracontrols.R \
	$(RDIR_analysis)/regressions_lessors.R \
	$(RDIR_analysis)/regressions_outputs.R \
	$(RDIR_analysis)/regressions_parcels.R \
	$(RDIR_analysis)/size_het.R: \
	$(RDIR)/functions/regtable.R

# ===========================================================================
# Output final paper
# ===========================================================================
$(PAPER).pdf $(DIR)/writeups/cs_texas_combined.pdf: \
	$(PAPER).tex $(PAPER).bib $(APPENDIX).tex \
	$(DIR)/writeups/paper_content.tex \
	$(DIR)/writeups/appendix_content.tex \
	$(DIR)/writeups/cs_texas_combined.tex \
	$(FDIR)/cohorts.png \
	$(FDIR)/sample_glo_leases.png \
	$(FDIR)/active_plot.png \
	$(FDIR)/glo_leases_in_texas.png \
	$(EDIR)/Bonus_Grid10Yr_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log_total.tex \
	$(EDIR)/Poisson_LeaseRevenue_Grid10Yr.tex \
	$(EDIR)/SellerRevenue_Grid10Yr_total.tex \
	$(EDIR)/Nsample_RAL_RAW.tex \
	$(EDIR)/Nsample_STATE_RAW.tex \
	$(EDIR)/Nsample_NEGOTIATION_CLEAN.tex \
	$(EDIR)/Nsample_AUCTION_CLEAN.tex \
	$(TDIR)/summary_stats_by_type.tex \
	$(EDIR)/Nsample_NEGOTIATION_NOTHICK.tex \
	$(EDIR)/Nsample_AUCTION_NOTHICK.tex \
	$(TDIR)/parcel_balance.tex \
	$(EDIR)/Bonus_Grid10_log.tex \
	$(TDIR)/logbonus_regressions.tex \
	$(EDIR)/negotiation_avg_bonus.tex \
	$(TDIR)/royalty_term_regressions.tex \
	$(EDIR)/lease_rev_min.tex \
	$(EDIR)/lease_rev_max.tex \
	$(EDIR)/negotiation_avg_revenue.tex \
	$(TDIR)/stacked_output_levels.tex \
	$(TDIR)/stacked_output_poisson.tex \
	$(EDIR)/negotiation_avg_dboe.tex \
	$(EDIR)/dboe_min.tex \
	$(EDIR)/dboe_max.tex \
	$(EDIR)/sellerrev_min.tex \
	$(EDIR)/sellerrev_max.tex \
	$(TDIR)/summary_stats_parcel.tex \
	$(EDIR)/tract_date_leased_share.tex \
	$(EDIR)/tract_ever_leased_share.tex \
	$(TDIR)/parcel_regressions.tex \
	$(EDIR)/ever_bidder_negotiation_share.tex \
	$(EDIR)/negotiation_winner_auction_share.tex \
	$(TDIR)/allocative.tex \
	$(TDIR)/firms_regressions.tex \
	$(TDIR)/TopPairAuctionShares.tex \
	$(EDIR)/tract_date_leased_insample.tex \
	$(TDIR)/auction_number_bids.tex \
	$(TDIR)/auction_bonus_regressions.tex \
	$(EDIR)/drop_bidder_v_21.tex \
	$(EDIR)/drop_bidder_v_32.tex \
	$(EDIR)/drop_bidder_v_31.tex \
	$(TDIR)/bonus_regressions.tex \
	$(TDIR)/lease_regressions_extra_bonus.tex \
	$(TDIR)/lease_regressions_extra_output.tex \
	$(TDIR)/logbonus_regressions_lessor_heterogeneity.tex \
	$(TDIR)/logbonus_size_heterogeneity.tex \
	$(EDIR)/negotiation_med_size.tex \
	$(TDIR)/lease_parcel_comparisons_linear.tex \
	$(TDIR)/lease_parcel_comparisons_poisson.tex \
	$(TDIR)/summary_data_construction.tex \
	$(TDIR)/auction_appendix.tex \
	$(TDIR)/stacked_regressions_addenda.tex \
	$(TDIR)/spell_stats.tex \
	$(TDIR)/logrank_stats.tex \
	$(TDIR)/drilled_regressions.tex \
	$(TDIR)/logdboe_drilled_regressions.tex \
	$(FDIR)/ipwkm10.png \
	$(FDIR)/ipwkm20.png
	cd writeups && pdflatex $< && bibtex cs_texas.aux && \
	pdflatex $< && pdflatex $< && pdflatex cs_texas_appendix && \
	pdflatex $< && pdflatex cs_texas_appendix && \
	bibtex cs_texas_appendix.aux && \
	pdflatex cs_texas_appendix && pdflatex cs_texas_appendix && \
	pdflatex cs_texas_combined && bibtex cs_texas_combined.aux && \
	pdflatex cs_texas_combined && pdflatex cs_texas_combined







{
  gROOT->Reset();

  f = new TFile("glrun20.chkmc.root");
  f->ls();

  c1 = new TCanvas("c1", "", 10, 10, 400, 380);

  c1->SetGridx();
  c1->SetGridy();
  c1->cd();

  //  ntuple->Draw("log(energy)");

  ntuple->Draw("log10(energy)>>hefit");

  Stat_t n;
  Int_t i,nbin;
  Stat_t v;

  nbin=hefit->GetNbinsX();

  for (i=0; i<nbin; ++i) {
	v = hefit->GetBinContent(i);
	if ( v > 0.0 ) 
	  v = log10( v );
	hefit->SetBinContent(i, v);
  }
  

  hefit->Fit("pol1");
  hefit->Draw();
  
  c1->Update();
  //  f->Close();
}

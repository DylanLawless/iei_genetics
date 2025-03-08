# set up URLs ----
# These URLs are only to show the text, but do not get used for the actual hyperlink. I add those within
# When using % for url, we nee to escape it for sprintf. Use doulbe (%%).
# We also have to escape for the reactable, and therefore use quadruple (%%%%).
df$UniProt <- sprintf('uniprot.org/%s', df$`Gene symbol`)
df$Ensembl <- sprintf('ensembl.org/%s', df$`Gene symbol`)
df$OMIM <- sprintf('omim.org/%s', df$`Gene symbol`)
df$omni <- sprintf('omni/%s', df$`Gene symbol`)
df$'gnomAD r2 GRCh37' <- sprintf('gnomad.org/r2%s', df$`Gene symbol`)
df$'gnomAD r3 GRCh38' <- sprintf('gnomad.org/r3%s', df$`Gene symbol`)
df$pdb <- sprintf('rcsb.org/%s', df$`Gene symbol`)
df$HGNC <- sprintf('genenames.org/%s', df$`Gene symbol`)
df$ORPHA <- sprintf('hpo/ORPHA/%s', df$`Gene symbol`)
df$HPO <- sprintf('hpo.jax.org/%s', df$`Gene symbol`)
# df$DisProt <- sprintf('disprot.org/%s', df$`Gene symbol`)
df$AmiGo <- sprintf('amigo.org/%s', df$`Gene symbol`)
df$'Alpha Fold' <-  sprintf('alphafold/%s', df$`Gene symbol`)
df$Gemma <- sprintf('gemma/%s', df$`Gene symbol`)
df$ClinGen <- sprintf('clinicalgenome.org/%s', df$`Gene symbol`)

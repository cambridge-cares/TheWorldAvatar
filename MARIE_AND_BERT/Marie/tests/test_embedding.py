def test_triple_distance(self):
    test_entity = ""
    rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                header=None)
    ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                header=None)

    attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                 header=None)

    bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                 header=None)

    relation_idx = 110
    print("relation label: ", self.idx2rel[relation_idx])
    rel = torch.tensor(rel_embedding.iloc[relation_idx].values).to(self.device)
    attr = torch.tensor(attr_embedding.iloc[relation_idx].values).to(self.device)
    bias = torch.tensor(bias_embedding.iloc[relation_idx].values).to(self.device)
    head_idx = self.entity2idx[test_entity]
    heads_idx = torch.Tensor([head_idx])
    tails_idx = torch.Tensor(self.subgraph_extractor.extract_neighbour_from_idx(head_idx))

    head = torch.tensor(ent_embedding.iloc[heads_idx].values).to(self.device)
    tails = torch.tensor(ent_embedding.iloc[tails_idx].values).to(self.device)

    repeat_num = len(tails)
    heads = head.repeat(repeat_num, 1).to(self.device)
    rels = rel.repeat(repeat_num, 1).to(self.device)
    # ae7b1072382624c77c75c9cb516cc29969680ad1 / 33.98
    aV = self.score_model.get_numerical_prediction(head=head, attr=attr, bias=bias)
    numerical_prediction = aV
    # numerical_prediction =  (aV + bias) * 100
    print("numerical_prediction", numerical_prediction)

    scores = self.score_model.triple_distance(heads, tails, rels)
    _, indices_top_k = torch.topk(scores, k=len(scores), largest=self.largest)
    labels_top_k = [self.idx2entity[tails_idx[index].item()] for index in indices_top_k]
    print(labels_top_k)
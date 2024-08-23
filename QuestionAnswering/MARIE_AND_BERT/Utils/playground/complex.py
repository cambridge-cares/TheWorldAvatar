from pykg2vec.models.KGMeta import PointwiseModel
from pykg2vec.utils.trainer import Trainer

class Complex(PointwiseModel):
    """
        `Complex Embeddings for Simple Link Prediction`_ (ComplEx) is an enhanced version of DistMult in that it uses complex-valued embeddings
        to represent both entities and relations. Using the complex-valued embedding allows
        the defined scoring function in ComplEx to differentiate that facts with assymmetric relations.

        Args:
            config (object): Model configuration parameters.

        .. _Complex Embeddings for Simple Link Prediction:
            http://proceedings.mlr.press/v48/trouillon16.pdf

    """
    def __init__(self, **kwargs):
        super(Complex, self).__init__(self.__class__.__name__.lower())
        param_list = ["tot_entity", "tot_relation", "hidden_size", "lmbda"]
        param_dict = self.load_params(param_list, kwargs)
        self.__dict__.update(param_dict)

        num_total_ent = self.tot_entity
        num_total_rel = self.tot_relation
        k = self.hidden_size

        self.ent_embeddings_real = NamedEmbedding("emb_e_real", num_total_ent, k)
        self.ent_embeddings_img = NamedEmbedding("emb_e_img", num_total_ent, k)
        self.rel_embeddings_real = NamedEmbedding("emb_rel_real", num_total_rel, k)
        self.rel_embeddings_img = NamedEmbedding("emb_rel_img", num_total_rel, k)
        nn.init.xavier_uniform_(self.ent_embeddings_real.weight)
        nn.init.xavier_uniform_(self.ent_embeddings_img.weight)
        nn.init.xavier_uniform_(self.rel_embeddings_real.weight)
        nn.init.xavier_uniform_(self.rel_embeddings_img.weight)

        self.parameter_list = [
            self.ent_embeddings_real,
            self.ent_embeddings_img,
            self.rel_embeddings_real,
            self.rel_embeddings_img,
        ]

        self.loss = Criterion.pointwise_logistic

    def embed(self, h, r, t):
        """Function to get the embedding value.

           Args:
               h (Tensor): Head entities ids.
               r (Tensor): Relation ids of the triple.
               t (Tensor): Tail entity ids of the triple.

            Returns:
                Tensors: Returns real and imaginary values of head, relation and tail embedding.
        """
        h_emb_real = self.ent_embeddings_real(h)
        h_emb_img = self.ent_embeddings_img(h)

        r_emb_real = self.rel_embeddings_real(r)
        r_emb_img = self.rel_embeddings_img(r)

        t_emb_real = self.ent_embeddings_real(t)
        t_emb_img = self.ent_embeddings_img(t)

        return h_emb_real, h_emb_img, r_emb_real, r_emb_img, t_emb_real, t_emb_img

    def forward(self, h, r, t):
        h_e_real, h_e_img, r_e_real, r_e_img, t_e_real, t_e_img = self.embed(h, r, t)
        return -torch.sum(h_e_real * t_e_real * r_e_real + h_e_img * t_e_img * r_e_real +
                          h_e_real * t_e_img * r_e_img - h_e_img * t_e_real * r_e_img, -1)

    def get_reg(self, h, r, t, reg_type="F2"):
        h_e_real, h_e_img, r_e_real, r_e_img, t_e_real, t_e_img = self.embed(h, r, t)

        if reg_type.lower() == 'f2':
            regul_term = torch.mean(torch.sum(h_e_real ** 2, -1) + torch.sum(h_e_img ** 2, -1) + torch.sum(r_e_real ** 2, -1) +
                                    torch.sum(r_e_img ** 2, -1) + torch.sum(t_e_real ** 2, -1) + torch.sum(t_e_img ** 2, -1))
        elif reg_type.lower() == 'n3':
            regul_term = torch.mean(torch.sum(h_e_real ** 3, -1) + torch.sum(h_e_img ** 3, -1) + torch.sum(r_e_real ** 3, -1) +
                                    torch.sum(r_e_img ** 3, -1) + torch.sum(t_e_real ** 3, -1) + torch.sum(t_e_img ** 3, -1))
        else:
            raise NotImplementedError('Unknown regularizer type: %s' % reg_type)

        return self.lmbda*regul_term
